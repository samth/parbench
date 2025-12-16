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
         "../common/run.rkt")

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

;; Parallel MCSS using reduction with tuple (best, prefix, suffix, total)
;; This properly handles subarrays crossing chunk boundaries
;; WARNING: Parallel version is SLOWER than sequential due to allocation
;; overhead from creating tuple vectors. Sequential Kadane's is O(n) with
;; constant space, while parallel requires O(n) tuple allocations.
(define (mcss-parallel data workers)
  (define n (vector-length data))
  (when (= n 0) (error 'mcss "empty data"))

  ;; Tuple representation: (best prefix suffix total)
  ;; For single element x: (x, x, x, x)
  (define (make-tuple x) (vector x x x x))
  (define (tuple-best t) (vector-ref t 0))
  (define (tuple-prefix t) (vector-ref t 1))
  (define (tuple-suffix t) (vector-ref t 2))
  (define (tuple-total t) (vector-ref t 3))

  ;; Combine two tuples - associative operation for reduction
  (define (combine t1 t2)
    (define best1 (tuple-best t1))
    (define prefix1 (tuple-prefix t1))
    (define suffix1 (tuple-suffix t1))
    (define total1 (tuple-total t1))
    (define best2 (tuple-best t2))
    (define prefix2 (tuple-prefix t2))
    (define suffix2 (tuple-suffix t2))
    (define total2 (tuple-total t2))
    (vector (max best1 best2 (+ suffix1 prefix2))           ; best spans boundary
            (max prefix1 (+ total1 prefix2))                ; prefix extends into right
            (max suffix2 (+ suffix1 total2))                ; suffix extends into left
            (+ total1 total2)))                             ; total sum

  ;; Process a range and return combined tuple
  (define (process-range start end)
    (let loop ([i (add1 start)]
               [acc (make-tuple (vector-ref data start))])
      (if (>= i end)
          acc
          (loop (add1 i) (combine acc (make-tuple (vector-ref data i)))))))

  (define pool (make-parallel-thread-pool workers))
  (define chunk-size (max 1 (quotient (+ n workers -1) workers)))

  (define channels
    (for/list ([start (in-range 0 n chunk-size)])
      (define end (min n (+ start chunk-size)))
      (define ch (make-channel))
      (thread #:pool pool (λ () (channel-put ch (process-range start end))))
      ch))

  (define partial-results (map channel-get channels))
  (parallel-thread-pool-close pool)

  ;; Reduce partial results
  (define final-tuple
    (for/fold ([acc (first partial-results)])
              ([t (in-list (rest partial-results))])
      (combine acc t)))

  (tuple-best final-tuple))

(module+ main
  (define n 1000000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")
  (define skip-sequential #f)


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
    (set! log-path arg)]
   [("--skip-sequential") "Skip sequential variant"
    (set! skip-sequential #t)])

  ;; Generate input data
  (define data (generate-random-data n seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'seed seed)
                       (list 'workers workers)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential mcss(n=~a)...\n" n)
    (set! seq-result
      (run-benchmark
       (λ () (mcss-sequential data))
       #:name 'mcss
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

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
               (when (and seq-result (not (< (abs (- seq-result result)) 0.001)))
                 (error 'mcss "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (< (abs (- seq-result par-result)) 0.001)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "mcss = ~a\n" par-result))
