#lang racket

(require racket/future
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide spectral-norm)

;; --- Matrix helper ---

(define (eval-A i j)
  (define ij (+ i j))
  (/ 1.0 (+ (* 0.5 ij (+ ij 1)) i 1)))

(define (make-vector n [fill 0.0])
  (for/vector ([i n]) fill))

(define (apply-operator vec workers element-fn)
  (define n (vector-length vec))
  (define result (make-vector n))
  (define worker-count (max 1 workers))
  (define chunk-size (max 1 (ceiling (/ n worker-count))))
  (cond
    [(= worker-count 1)
     (for ([i n])
       (define sum 0.0)
       (for ([j n])
         (set! sum (+ sum (* (element-fn i j) (vector-ref vec j)))))
       (vector-set! result i sum))]
    [else
     (define ranges (for/list ([start (in-range 0 n chunk-size)])
                      (cons start (min n (+ start chunk-size)))))
    (define futures
      (for/list ([rg (in-list ranges)])
        (define start (car rg))
        (define end (cdr rg))
        (future (λ ()
                  (define partial (make-vector (- end start)))
                  (for ([i (in-range start end)] [offset (in-naturals)])
                    (define sum 0.0)
                    (for ([j n])
                      (set! sum (+ sum (* (element-fn i j) (vector-ref vec j)))))
                    (vector-set! partial offset sum))
                  (cons start partial)))))
    (for ([f futures])
      (define start+partial (touch f))
      (define start (car start+partial))
      (define partial (cdr start+partial))
      (for ([offset (in-naturals)]
            [value (in-vector partial)])
        (vector-set! result (+ start offset) value)))])
  result)

(define (apply-Au vec workers)
  (apply-operator vec workers eval-A))

(define (apply-Atv vec workers)
  (apply-operator vec workers (λ (i j) (eval-A j i))))

(define (spectral-norm n #:workers [workers 1] #:iterations [iterations 10])
  (define worker-count (max 1 workers))
  (define u (for/vector ([i n]) 1.0))
  (define v (make-vector n 0.0))
  (for ([iter iterations])
    (set! v (apply-Au u worker-count))
    (set! u (apply-Atv v worker-count)))
  (define vBv (for/sum ([i n]) (* (vector-ref u i) (vector-ref v i))))
  (define vv (for/sum ([i n]) (* (vector-ref v i) (vector-ref v i))))
  (sqrt (/ vBv vv)))

(module+ main
  (define n 1000)
  (define workers (processor-count))
  (define iterations 10)
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "spectral-norm.rkt"
    #:once-each
    [("--n") arg "Matrix dimension"
     (set! n (parse-positive-integer arg 'spectral-norm))]
    [("--iterations" "-i") arg "Power method iterations"
     (set! iterations (parse-positive-integer arg 'spectral-norm))]
    [("--workers") arg "Parallel futures"
     (set! workers (parse-positive-integer arg 'spectral-norm))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'spectral-norm))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'iterations iterations)
                       (list 'workers workers)))

  (define sequential-value
    (run-benchmark
     (λ () (spectral-norm n #:workers 1 #:iterations iterations))
     #:name 'spectral-norm
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (spectral-norm n #:workers workers #:iterations iterations))
   #:name 'spectral-norm
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (λ (_ value)
             (define diff (abs (- value sequential-value)))
             (when (> diff 1e-6)
               (error 'spectral-norm "parallel mismatch"))))

  (close-log-writer writer))
