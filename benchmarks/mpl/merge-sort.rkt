#lang racket

;; Port of msort from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/msort
;; Adapted for Racket parallel benchmarking
;;
;; Merge Sort: Classic divide-and-conquer sorting algorithm.
;; Parallel version uses a thread pool to sort halves concurrently.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide merge-sort-sequential
         merge-sort-parallel
         generate-random-vector)

;; Generate random vector for testing
(define (generate-random-vector n seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (for/vector ([i (in-range n)])
      (random 1000000))))

;; Merge two sorted vectors
(define (merge left right)
  (define left-len (vector-length left))
  (define right-len (vector-length right))
  (define result (make-vector (+ left-len right-len)))

  (let loop ([i 0] [j 0] [k 0])
    (cond
      [(>= i left-len)
       ;; Copy remaining from right
       (for ([idx (in-range j right-len)])
         (vector-set! result (+ k (- idx j)) (vector-ref right idx)))
       result]
      [(>= j right-len)
       ;; Copy remaining from left
       (for ([idx (in-range i left-len)])
         (vector-set! result (+ k (- idx i)) (vector-ref left idx)))
       result]
      [else
       (define left-val (vector-ref left i))
       (define right-val (vector-ref right j))
       (if (<= left-val right-val)
           (begin
             (vector-set! result k left-val)
             (loop (add1 i) j (add1 k)))
           (begin
             (vector-set! result k right-val)
             (loop i (add1 j) (add1 k))))])))

;; Sequential merge sort
(define (merge-sort-sequential vec)
  (define n (vector-length vec))
  (cond
    [(<= n 1) vec]
    [else
     (define mid (quotient n 2))
     (define left (merge-sort-sequential (vector-copy vec 0 mid)))
     (define right (merge-sort-sequential (vector-copy vec mid)))
     (merge left right)]))

;; Parallel merge sort with threshold
(define (merge-sort-parallel vec workers [threshold 1000])
  (define n (vector-length vec))

  (define (sorted-subvector v)
    (merge-sort-sequential v))

  (if (<= workers 1)
      (merge-sort-sequential vec)
      (call-with-thread-pool workers
        (λ (pool actual-workers)
          (define (parallel-merge-sort v depth)
            (define len (vector-length v))
            (cond
              [(<= len threshold) (sorted-subvector v)]
              [(<= len 1) v]
              [else
               (define mid (quotient len 2))
               (define left-vec (vector-copy v 0 mid))
               (define right-vec (vector-copy v mid))
               (if (< depth 3)
                   (let ([left-task (thread-pool-submit pool
                                                        (λ () (parallel-merge-sort left-vec (add1 depth))))])
                     (define right-sorted (parallel-merge-sort right-vec (add1 depth)))
                     (define left-sorted (thread-pool-wait left-task))
                     (merge left-sorted right-sorted))
                   (let ([left-sorted (parallel-merge-sort left-vec (add1 depth))]
                         [right-sorted (parallel-merge-sort right-vec (add1 depth))])
                     (merge left-sorted right-sorted)))]))
          (parallel-merge-sort vec 0))
        #:max #f)))

(module+ main
  (define n 1000000)
  (define seed 42)
  (define workers 1)
  (define threshold 1000)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "merge-sort"
   #:once-each
   [("--n") arg "Number of elements (default: 1000000)"
    (set! n (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--threshold") arg "Sequential threshold (default: 1000)"
    (set! threshold (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)])

  ;; Generate input data
  (printf "Generating random vector of size ~a...\n" n)
  (define data (generate-random-vector n seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'seed seed)
                       (list 'threshold threshold)
                       (list 'workers workers)))

  (printf "Running sequential merge-sort(n=~a)...\n" n)
  (define seq-result
    (run-benchmark
     (λ () (merge-sort-sequential data))
     #:name 'merge-sort
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel merge-sort(n=~a) (workers=~a, threshold=~a)...\n" n workers threshold)
  (define par-result
    (run-benchmark
     (λ () (merge-sort-parallel data workers threshold))
     #:name 'merge-sort
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (equal? seq-result result)
                 (error 'merge-sort "parallel result mismatch at iteration ~a"
                        iteration)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (equal? seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "Sorted ~a elements\n" n))
