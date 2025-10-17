#lang racket

;; Port of fib from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/fib
;; Adapted for Racket parallel benchmarking
;;
;; Naive recursive parallel Fibonacci - useful for testing scheduler overhead
;; and trivial parallelism patterns.

(require "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide fib-sequential
         fib-parallel)

;; Sequential naive recursive Fibonacci
(define (fib-sequential n)
  (if (<= n 1)
      n
      (+ (fib-sequential (- n 1))
         (fib-sequential (- n 2)))))

;; Parallel naive recursive Fibonacci with cutoff threshold
;; Below threshold, use sequential to avoid overhead
(define (fib-parallel n workers [threshold 20])
  (call-with-thread-pool workers
    (λ (pool actual-workers)
      (define (fib-par n)
        (if (<= n threshold)
            (fib-sequential n)
            (let ([f1 (thread-pool-submit pool (λ () (fib-par (- n 1))))]
                  [f2 (fib-par (- n 2))])
              (+ (thread-pool-result-value (thread-pool-wait f1))
                 f2))))
      (fib-par n))
    #:max (expt 2 (max 0 (- n threshold)))))

;; Known Fibonacci values for verification
(define fib-known
  #hash((0 . 0) (1 . 1) (2 . 1) (3 . 2) (4 . 3) (5 . 5) (6 . 8) (7 . 13)
        (8 . 21) (9 . 34) (10 . 55) (11 . 89) (12 . 144) (13 . 233) (14 . 377)
        (15 . 610) (16 . 987) (17 . 1597) (18 . 2584) (19 . 4181) (20 . 6765)
        (25 . 75025) (30 . 832040) (35 . 9227465) (40 . 102334155)))

(module+ main
  (define n 30)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define threshold 20)

  (void
   (command-line
    #:program "fib.rkt"
    #:once-each
    [("--n") arg "Fibonacci number to compute"
     (set! n (parse-positive-integer arg 'fib))]
    [("--threshold") arg "Sequential cutoff threshold"
     (set! threshold (parse-positive-integer arg 'fib))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'fib))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'fib))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'threshold threshold)
                       (list 'workers workers)))

  (printf "Running sequential fib(~a)...\n" n)
  (define seq-result
    (run-benchmark
     (λ () (fib-sequential n))
     #:name 'fib
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel fib(~a) (workers=~a, threshold=~a)...\n" n workers threshold)
  (define par-result
    (run-benchmark
     (λ () (fib-parallel n workers threshold))
     #:name 'fib
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (= seq-result result)
                 (error 'fib "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (= seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "fib(~a) = ~a\n" n seq-result)

  ;; Check against known value if available
  (when (hash-has-key? fib-known n)
    (define expected (hash-ref fib-known n))
    (if (= seq-result expected)
        (printf "✓ Result matches known value\n")
        (printf "✗ Result ~a does not match expected ~a!\n" seq-result expected))))
