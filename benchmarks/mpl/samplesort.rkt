#lang racket

;; Port of samplesort from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/samplesort
;; Adapted for Racket parallel benchmarking
;;
;; Sample Sort: Parallel sorting algorithm that uses sampling to partition data.
;; 1. Select random samples from input
;; 2. Sort samples to determine bucket boundaries (splitters)
;; 3. Partition input into buckets based on splitters
;; 4. Sort each bucket in parallel
;; 5. Concatenate sorted buckets

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide samplesort-sequential
         samplesort-parallel
         generate-random-vector)

;; Generate random vector for testing
(define (generate-random-vector n seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (for/vector ([i (in-range n)])
      (random 1000000))))

;; Sequential quicksort (used for small buckets)
(define (quicksort vec)
  (define n (vector-length vec))
  (cond
    [(<= n 1) vec]
    [else
     (define pivot (vector-ref vec (quotient n 2)))
     ;; Count partition sizes
     (define-values (left-count equal-count right-count)
       (for/fold ([left-c 0] [equal-c 0] [right-c 0])
                 ([x (in-vector vec)])
         (cond
           [(< x pivot) (values (+ left-c 1) equal-c right-c)]
           [(> x pivot) (values left-c equal-c (+ right-c 1))]
           [else (values left-c (+ equal-c 1) right-c)])))
     ;; Allocate partition vectors
     (define left (make-vector left-count))
     (define equal (make-vector equal-count))
     (define right (make-vector right-count))
     ;; Fill partitions
     (define left-pos 0)
     (define equal-pos 0)
     (define right-pos 0)
     (for ([x (in-vector vec)])
       (cond
         [(< x pivot)
          (vector-set! left left-pos x)
          (set! left-pos (+ left-pos 1))]
         [(> x pivot)
          (vector-set! right right-pos x)
          (set! right-pos (+ right-pos 1))]
         [else
          (vector-set! equal equal-pos x)
          (set! equal-pos (+ equal-pos 1))]))
     (vector-append (quicksort left) equal (quicksort right))]))

;; Sequential samplesort (just use quicksort)
(define (samplesort-sequential vec)
  (quicksort vec))

;; Parallel samplesort
(define (samplesort-parallel vec workers)
  (define n (vector-length vec))

  (cond
    [(<= n 1000) (quicksort vec)]  ; Use sequential for small inputs
    [else
     ;; Step 1: Select samples (oversample by factor of workers)
     (define num-samples (* workers workers))
     (define samples
       (for/vector ([i (in-range num-samples)])
         (vector-ref vec (random n))))

     ;; Step 2: Sort samples to get splitters
     (define sorted-samples (quicksort samples))
     (define splitters
       (for/vector ([i (in-range 1 workers)])
         (vector-ref sorted-samples (* i workers))))

     ;; Step 3a: Count elements per bucket
     (define bucket-counts (make-vector workers 0))
     (for ([elem (in-vector vec)])
       (define bucket-idx
         (let loop ([idx 0])
           (cond
             [(>= idx (vector-length splitters)) idx]
             [(< elem (vector-ref splitters idx)) idx]
             [else (loop (add1 idx))])))
       (vector-set! bucket-counts bucket-idx
                    (+ 1 (vector-ref bucket-counts bucket-idx))))

     ;; Step 3b: Allocate bucket vectors
     (define buckets
       (for/vector ([count (in-vector bucket-counts)])
         (make-vector count)))

     ;; Step 3c: Fill buckets
     (define bucket-positions (make-vector workers 0))
     (for ([elem (in-vector vec)])
       (define bucket-idx
         (let loop ([idx 0])
           (cond
             [(>= idx (vector-length splitters)) idx]
             [(< elem (vector-ref splitters idx)) idx]
             [else (loop (add1 idx))])))
       (define pos (vector-ref bucket-positions bucket-idx))
       (define bucket (vector-ref buckets bucket-idx))
       (vector-set! bucket pos elem)
       (vector-set! bucket-positions bucket-idx (+ pos 1)))

     ;; Step 4: Sort each bucket in parallel
     (define sorted-buckets
       (if (<= workers 1)
           (for/list ([bucket (in-vector buckets)])
             (quicksort bucket))
           (call-with-thread-pool workers
             (λ (pool actual-workers)
               (define tasks
                 (for/list ([bucket (in-vector buckets)])
                   (thread-pool-submit pool
                                       (λ () (quicksort bucket)))))
               (thread-pool-wait/collect tasks))
             #:max workers)))

     ;; Step 5: Concatenate sorted buckets
     (apply vector-append sorted-buckets)]))

(module+ main
  (define n 1000000)
  (define seed 42)
  (define workers 4)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "samplesort"
   #:once-each
   [("--n") arg "Number of elements (default: 1000000)"
    (set! n (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--workers") arg "Number of workers (default: 4)"
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
                       (list 'workers workers)))

  (printf "Running sequential samplesort(n=~a)...\n" n)
  (define seq-result
    (run-benchmark
     (λ () (samplesort-sequential data))
     #:name 'samplesort
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel samplesort(n=~a) (workers=~a)...\n" n workers)
  (define par-result
    (run-benchmark
     (λ () (samplesort-parallel data workers))
     #:name 'samplesort
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (equal? seq-result result)
                 (error 'samplesort "parallel result mismatch at iteration ~a"
                        iteration)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (equal? seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "Sorted ~a elements\n" n))
