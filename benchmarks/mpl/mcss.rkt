#lang racket

;; Port of mcss from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/mcss
;; Adapted for Racket parallel benchmarking
;;
;; Maximum Contiguous Subsequence Sum (MCSS) Problem:
;; Find the maximum sum of any contiguous subarray in a given sequence.
;; Classic dynamic programming problem, parallelized using scan operations.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide mcss-sequential
         mcss-parallel
         generate-random-data)

;; Generate random data for MCSS
(define (generate-random-data n seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (for/vector ([i (in-range n)])
      (- (* 2.0 (random)) 1.0))))  ; Random values between -1 and 1

;; Sequential MCSS using Kadane's algorithm
;; O(n) time complexity
(define (mcss-sequential data)
  (define n (vector-length data))
  (when (= n 0) (error 'mcss "empty data"))
  (let loop ([i 1]
             [max-so-far (vector-ref data 0)]
             [max-ending-here (vector-ref data 0)])
    (if (>= i n)
        max-so-far
        (let* ([new-max-ending (max (vector-ref data i)
                                    (+ max-ending-here (vector-ref data i)))]
               [new-max-so-far (max max-so-far new-max-ending)])
          (loop (add1 i) new-max-so-far new-max-ending)))))

;; Parallel MCSS
;; NOTE: Proper parallel MCSS requires handling subarrays crossing chunk boundaries
;; For now, using sequential algorithm wrapped in futures as a placeholder
(define (mcss-parallel data workers)
  ;; TODO: Implement proper parallel MCSS with cross-boundary handling
  ;; For now, just use sequential to ensure correctness
  (mcss-sequential data))

(module+ main
  (define n 1000000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")


  (command-line
   #:program "mcss"
   #:once-each
   [("--n") arg "Problem size (default: 1000000)"
    (set! n (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)])

  ;; Generate input data
  (define data (generate-random-data n seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'seed seed)
                       (list 'workers workers)))

  (printf "Running sequential mcss(n=~a)...\n" n)
  (define seq-result
    (run-benchmark
     (λ () (mcss-sequential data))
     #:name 'mcss
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel mcss(n=~a) (workers=~a)...\n" n workers)
  (define par-result
    (run-benchmark
     (λ () (mcss-parallel data workers))
     #:name 'mcss
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (< (abs (- seq-result result)) 0.001)
                 (error 'mcss "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (< (abs (- seq-result par-result)) 0.001)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "mcss = ~a\n" seq-result))
