#lang racket

(require racket/fixnum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide integer-sort-sequential
         integer-sort-parallel)

;; ============================================================================
;; Parallel Primitives
;; ============================================================================

(define GRAIN 10000)

;; Parallel for-each over range
(define (parallel-for-each f n workers)
  (if (< n GRAIN)
      (for ([i (in-range n)]) (f i))
      (let* ([chunk-size (quotient (+ n workers -1) workers)]
             [threads
              (for/list ([w (in-range workers)])
                (define start (* w chunk-size))
                (define end (min (+ start chunk-size) n))
                (if (>= start n)
                    #f
                    (thread (lambda ()
                              (for ([i (in-range start end)]) (f i))))))])
        (for ([t threads] #:when t) (thread-wait t)))))

;; Parallel prefix sum (exclusive scan)
;; Returns vector where result[i] = sum of all elements before i
(define (parallel-prefix-sum vec workers)
  (define n (vector-length vec))
  (if (< n (* 2 workers))
      ;; Sequential
      (let ([result (make-vector n 0)])
        (for ([i (in-range 1 n)])
          (vector-set! result i (fx+ (vector-ref result (fx- i 1))
                                      (vector-ref vec (fx- i 1)))))
        result)
      ;; Parallel: block-based prefix sum
      (let* ([block-size (quotient (+ n workers -1) workers)]
             ;; Phase 1: compute local sums per block
             [block-sums
              (let ([channels
                     (for/list ([w (in-range workers)])
                       (define start (* w block-size))
                       (define end (min (+ start block-size) n))
                       (if (>= start n)
                           #f
                           (let ([ch (make-channel)])
                             (thread
                              (lambda ()
                                (define sum 0)
                                (for ([i (in-range start end)])
                                  (set! sum (fx+ sum (vector-ref vec i))))
                                (channel-put ch sum)))
                             ch)))])
                (for/vector ([ch channels])
                  (if ch (channel-get ch) 0)))]
             ;; Phase 2: sequential prefix sum of block sums
             [block-offsets (make-vector workers 0)]
             [_ (for ([i (in-range 1 workers)])
                  (vector-set! block-offsets i
                               (fx+ (vector-ref block-offsets (fx- i 1))
                                    (vector-ref block-sums (fx- i 1)))))]
             ;; Phase 3: parallel local prefix sums with offsets
             [result (make-vector n 0)])
        (let ([threads
               (for/list ([w (in-range workers)])
                 (define start (* w block-size))
                 (define end (min (+ start block-size) n))
                 (if (>= start n)
                     #f
                     (thread
                      (lambda ()
                        (define offset (vector-ref block-offsets w))
                        (when (> end start)
                          (vector-set! result start offset)
                          (for ([i (in-range (fx+ start 1) end)])
                            (vector-set! result i
                                         (fx+ (vector-ref result (fx- i 1))
                                              (vector-ref vec (fx- i 1))))))))))])
          (for ([t threads] #:when t) (thread-wait t)))
        result)))

;; ============================================================================
;; Sequential Counting Sort
;; ============================================================================

(define (integer-sort-sequential data range)
  (define n (vector-length data))
  (define counts (make-vector range 0))

  ;; Count occurrences
  (for ([val (in-vector data)])
    (vector-set! counts val (fx+ 1 (vector-ref counts val))))

  ;; Compute prefix sums (exclusive scan)
  (define positions (make-vector range 0))
  (for ([i (in-range 1 range)])
    (vector-set! positions i
                 (fx+ (vector-ref positions (fx- i 1))
                      (vector-ref counts (fx- i 1)))))

  ;; Place elements in sorted order
  (define result (make-vector n 0))
  (for ([val (in-vector data)])
    (define pos (vector-ref positions val))
    (vector-set! result pos val)
    (vector-set! positions val (fx+ pos 1)))

  result)

;; ============================================================================
;; Parallel Counting Sort with CAS-based scatter
;; ============================================================================

(define (integer-sort-parallel data range workers)
  (define n (vector-length data))
  (define chunk-size (quotient (+ n workers -1) workers))

  ;; Step 1: Parallel counting - each worker counts its partition
  (define local-counts-list
    (let ([channels
           (for/list ([w (in-range workers)])
             (define start (* w chunk-size))
             (define end (min (+ start chunk-size) n))
             (if (>= start n)
                 #f
                 (let ([ch (make-channel)])
                   (thread
                    (lambda ()
                      (define local-counts (make-vector range 0))
                      (for ([i (in-range start end)])
                        (define val (vector-ref data i))
                        (vector-set! local-counts val
                                     (fx+ 1 (vector-ref local-counts val))))
                      (channel-put ch local-counts)))
                   ch)))])
      (for/list ([ch channels] #:when ch) (channel-get ch))))

  ;; Step 2: Merge local counts (parallel reduce)
  (define global-counts (make-vector range 0))
  (for ([local-counts (in-list local-counts-list)])
    (for ([bucket (in-range range)])
      (vector-set! global-counts bucket
                   (fx+ (vector-ref global-counts bucket)
                        (vector-ref local-counts bucket)))))

  ;; Step 3: Parallel prefix sum to get positions
  (define positions (parallel-prefix-sum global-counts workers))

  ;; Step 4: Parallel scatter using CAS for position updates
  ;; We use a vector of atomic counters for each bucket
  (define write-positions (vector-copy positions))
  (define result (make-vector n 0))

  ;; Each worker scatters its portion using fetch-and-add semantics via CAS
  (let ([threads
         (for/list ([w (in-range workers)])
           (define start (* w chunk-size))
           (define end (min (+ start chunk-size) n))
           (if (>= start n)
               #f
               (thread
                (lambda ()
                  (for ([i (in-range start end)])
                    (define val (vector-ref data i))
                    ;; CAS loop to atomically increment write position
                    (let loop ()
                      (define old-pos (vector-ref write-positions val))
                      (if (unsafe-vector*-cas! write-positions val old-pos (fx+ old-pos 1))
                          (vector-set! result old-pos val)
                          (loop))))))))])
    (for ([t threads] #:when t) (thread-wait t)))

  result)

;; ============================================================================
;; Utilities
;; ============================================================================

(define (generate-random-data n range seed)
  (random-seed seed)
  (for/vector ([i (in-range n)])
    (random range)))

(define (verify-sorted data)
  (for/and ([i (in-range 1 (vector-length data))])
    (<= (vector-ref data (fx- i 1))
        (vector-ref data i))))

(define (verify-same-elements data1 data2)
  (and (= (vector-length data1) (vector-length data2))
       (equal? (sort (vector->list data1) <)
               (sort (vector->list data2) <))))

;; ============================================================================
;; Main
;; ============================================================================

(module+ main
  (define n 10000000)
  (define range 1000000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "integer-sort.rkt"
    #:once-each
    [("--n") arg "Number of elements"
     (set! n (parse-positive-integer arg 'integer-sort))]
    [("--range" "-r") arg "Maximum value (range of integers)"
     (set! range (parse-positive-integer arg 'integer-sort))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'integer-sort))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'integer-sort))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'integer-sort))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (printf "Generating random data (n=~a, range=[0,~a))...\n" n range)
  (define data (generate-random-data n range seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'range range)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential integer sort...\n")
    (set! seq-result
      (run-benchmark
       (lambda () (integer-sort-sequential data range))
       #:name 'integer-sort
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel integer sort (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (lambda () (integer-sort-parallel data range workers))
     #:name 'integer-sort
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification:\n")
    (printf "  Sequential sorted: ~a\n" (verify-sorted seq-result))
    (printf "  Parallel sorted: ~a\n" (verify-sorted par-result))
    (printf "  Same elements: ~a\n" (verify-same-elements seq-result par-result)))

  (printf "\nSample values (first 10):\n")
  (for ([i (in-range (min 10 n))])
    (printf "  result[~a] = ~a\n" i (vector-ref par-result i))))
