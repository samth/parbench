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
     (define-values (left equal right)
       (for/fold ([left '()] [equal '()] [right '()])
                 ([x (in-vector vec)])
         (cond
           [(< x pivot) (values (cons x left) equal right)]
           [(> x pivot) (values left equal (cons x right))]
           [else (values left (cons x equal) right)])))
     (vector-append (quicksort (list->vector left))
                    (list->vector equal)
                    (quicksort (list->vector right)))]))

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
       (for/list ([i (in-range 1 workers)])
         (vector-ref sorted-samples (* i workers))))

     ;; Step 3: Partition input into buckets
     (define buckets (make-vector workers '()))
     (for ([elem (in-vector vec)])
       (define bucket-idx
         (let loop ([idx 0] [splitters-list splitters])
           (cond
             [(null? splitters-list) idx]
             [(< elem (car splitters-list)) idx]
             [else (loop (add1 idx) (cdr splitters-list))])))
       (vector-set! buckets bucket-idx
                    (cons elem (vector-ref buckets bucket-idx))))

     ;; Step 4: Sort each bucket in parallel
     (define sorted-buckets
       (if (<= workers 1)
           (for/list ([bucket-list (in-vector buckets)])
             (quicksort (list->vector bucket-list)))
           (call-with-thread-pool workers
             (λ (pool actual-workers)
               (define tasks
                 (for/list ([bucket-list (in-vector buckets)])
                   (thread-pool-submit pool
                                       (λ () (quicksort (list->vector bucket-list))))))
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
