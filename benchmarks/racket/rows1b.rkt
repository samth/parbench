#lang racket

(require racket/place
         racket/string
         racket/math
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide rows1b-sequential
         rows1b-parallel
         rows1b-results=?
         rows1b-result->alist)

;; ---------------- Data Generation ----------------

(define cities
  '#("Amsterdam" "Berlin" "Cairo" "Denver" "Edinburgh" "Fukuoka" "Geneva" "Helsinki"
     "Istanbul" "Jakarta" "Kyoto" "Lisbon" "Mumbai" "Nairobi" "Oslo" "Paris"
     "Quito" "Rome" "Seoul" "Taipei" "Utrecht" "Vienna" "Warsaw" "Xiamen"
     "Yerevan" "Zurich"))

(define (temperature-int idx)
  (- (modulo (* idx 37) 2000) 1000)) ; range [-1000, 999] -> [-100.0, 99.9]

(define (temperature->string t)
  (define sign (if (negative? t) "-" ""))
  (define abs-t (abs t))
  (define whole (quotient abs-t 10))
  (define frac (remainder abs-t 10))
  (format "~a~a.~a" sign whole (abs frac)))

(define (generate-row idx)
  (define city (vector-ref cities (modulo idx (vector-length cities))))
  (define temp (temperature-int idx))
  (string-append city ";" (temperature->string temp)))

(define (find-separator row)
  (for/or ([i (in-range (string-length row))])
    (and (char=? (string-ref row i) #\;) i)))

(define (parse-row row)
  (define sep (or (find-separator row)
                  (error 'rows1b "invalid row: ~a" row)))
  (define city (substring row 0 sep))
  (define temp-str (substring row (add1 sep)))
  (define temp (inexact->exact (round (* 10 (string->number temp-str)))))
  (values city temp))

;; ---------------- Statistics ----------------

(struct city-stats (count sum min max) #:mutable)

(define (make-city-stats temp)
  (city-stats 1 temp temp temp))

(define (city-stats-add! stats temp)
  (set-city-stats-count! stats (add1 (city-stats-count stats)))
  (set-city-stats-sum! stats (+ (city-stats-sum stats) temp))
  (set-city-stats-min! stats (min (city-stats-min stats) temp))
  (set-city-stats-max! stats (max (city-stats-max stats) temp)))

(define (city-stats-merge! target source)
  (set-city-stats-count! target (+ (city-stats-count target) (city-stats-count source)))
  (set-city-stats-sum! target (+ (city-stats-sum target) (city-stats-sum source)))
  (set-city-stats-min! target (min (city-stats-min target) (city-stats-min source)))
  (set-city-stats-max! target (max (city-stats-max target) (city-stats-max source))))

(define (copy-city-stats st)
  (city-stats (city-stats-count st)
              (city-stats-sum st)
              (city-stats-min st)
              (city-stats-max st)))

(define (update-table! table city temp)
  (define existing (hash-ref table city #f))
  (if existing
      (city-stats-add! existing temp)
      (hash-set! table city (make-city-stats temp))))

(define (finalize-table table)
  (for/hash ([(city stats) (in-hash table)])
    (values city
            (vector (city-stats-min stats)
                    (city-stats-max stats)
                    (city-stats-sum stats)
                    (city-stats-count stats)))))

;; ---------------- Processing ----------------

(define (process-range start end)
  (define table (make-hash))
  (for ([idx (in-range start end)])
    (define row (generate-row idx))
    (define-values (city temp) (parse-row row))
    (update-table! table city temp))
  table)

(define (merge-tables! target source)
  (for ([(city stats) (in-hash source)])
    (define existing (hash-ref target city #f))
    (if existing
        (city-stats-merge! existing stats)
        (hash-set! target city (copy-city-stats stats)))))

(define (chunk-ranges total chunk-size)
  (for/list ([start (in-range 0 total chunk-size)])
    (cons start (min total (+ start chunk-size)))))

;; ---------------- Public API ----------------

(define (rows1b-sequential rows)
  (define table (process-range 0 rows))
  (finalize-table table))

(define (rows1b-parallel rows
                          #:workers [workers (processor-count)]
                          #:chunk-size [chunk-size #f])
  (define worker-count (max 1 workers))
  (define chunk (or chunk-size
                    (max 1 (ceiling (/ rows (* worker-count 2))))))
  (define ranges (chunk-ranges rows chunk))
  (define table (make-hash))
  (define results
    (call-with-thread-pool workers
      (λ (pool actual-workers)
        (thread-pool-wait/collect
         (for/list ([rg (in-list ranges)])
           (define start (car rg))
           (define end (cdr rg))
           (thread-pool-submit pool (λ () (process-range start end))))))
      #:max (length ranges)))
  (for ([result-values (in-list results)])
    (define chunk-table
      (cond
        [(and (list? result-values) (pair? result-values)) (first result-values)]
        [else result-values]))
    (merge-tables! table chunk-table))
  (finalize-table table))

(define (rows1b-results=? a b)
  (and (= (hash-count a) (hash-count b))
       (for/and ([(city stats-a) (in-hash a)])
         (define stats-b (hash-ref b city #f))
         (and stats-b (equal? stats-a stats-b)))))

(define (rows1b-result->alist result)
  (for/list ([(city stats) (in-hash result)])
    (list city stats)))

;; ---------------- Benchmark Harness ----------------

(module+ main
  (define rows 1000000)
  (define workers (processor-count))
  (define chunk-size #f)
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "rows1b.rkt"
    #:once-each
    [("--rows") arg "Number of generated rows to process."
     (set! rows (parse-positive-integer arg 'rows1b))]
    [("--workers") arg "Worker count for parallel variant."
     (set! workers (parse-positive-integer arg 'rows1b))]
    [("--chunk-size") arg "Chunk size (rows) processed per worker."
     (set! chunk-size (parse-positive-integer arg 'rows1b))]
    [("--repeat") arg "Number of benchmark repetitions."
     (set! repeat (parse-positive-integer arg 'rows1b))]
    [("--log") arg "Optional path for S-expression benchmark log."
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params-base (list (list 'rows rows)
                            (list 'workers workers)
                            (list 'chunk-size (or chunk-size 'auto))))

  (define sequential-reference
    (let ([holder #f])
      (run-benchmark
       (λ () (rows1b-sequential rows))
       #:name 'rows1b
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params-base
       #:metadata metadata
       #:check (λ (iteration result)
                 (cond
                   [(zero? iteration) (set! holder result)]
                   [else
                    (unless (rows1b-results=? result holder)
                      (error 'rows1b "sequential result changed"))])))
      holder))

  (run-benchmark
   (λ () (rows1b-parallel rows #:workers workers #:chunk-size chunk-size))
   #:name 'rows1b
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params-base
   #:metadata metadata
   #:check (λ (_ result)
             (unless (rows1b-results=? result sequential-reference)
               (error 'rows1b "parallel result mismatch"))))

  (close-log-writer writer))
