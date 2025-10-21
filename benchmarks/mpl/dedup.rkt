#lang racket

;; Port of dedup from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/dedup
;; Adapted for Racket parallel benchmarking
;;
;; Deduplication Problem:
;; Remove duplicate elements from a sequence while preserving order.
;; Uses hash-based bucketing for parallel deduplication.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide dedup-sequential
         dedup-parallel
         generate-data-with-duplicates)

;; Generate random data with duplicates
(define (generate-data-with-duplicates n num-unique seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (for/vector ([i (in-range n)])
      (random num-unique))))

;; Sequential deduplication using hash table
(define (dedup-sequential data)
  (define seen (make-hash))
  (define result (make-vector 0))
  (for ([elem (in-vector data)])
    (unless (hash-has-key? seen elem)
      (hash-set! seen elem #t)
      (set! result (vector-append result (vector elem)))))
  result)

;; Parallel deduplication using bucketing
(define (dedup-parallel data workers)
  (define n (vector-length data))
  (define num-buckets (* workers 4))  ; Use more buckets than workers

  ;; Step 1: Partition data into buckets based on hash
  (define buckets (make-vector num-buckets '()))
  (for ([elem (in-vector data)])
    (define bucket-idx (modulo (equal-hash-code elem) num-buckets))
    (vector-set! buckets bucket-idx
                 (cons elem (vector-ref buckets bucket-idx))))

  ;; Step 2: Deduplicate each bucket in parallel
  (define (dedupe-bucket bucket-list)
    (define seen (make-hash))
    (define deduped '())
    (for ([elem (in-list (reverse bucket-list))])  ; Reverse to preserve order
      (unless (hash-has-key? seen elem)
        (hash-set! seen elem #t)
        (set! deduped (cons elem deduped))))
    (reverse deduped))

  (define deduped-lists
    (if (<= workers 1)
        (for/list ([bucket-idx (in-range num-buckets)])
          (dedupe-bucket (vector-ref buckets bucket-idx)))
        (call-with-thread-pool workers
          (λ (pool actual-workers)
            (define tasks
              (for/list ([bucket-idx (in-range num-buckets)])
                (thread-pool-submit pool
                                    (λ () (dedupe-bucket (vector-ref buckets bucket-idx))))))
            (thread-pool-wait/collect tasks))
          #:max num-buckets)))

  ;; Step 4: Flatten results
  (list->vector (apply append deduped-lists)))

(module+ main
  (define n 1000000)
  (define num-unique 100000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "dedup"
   #:once-each
   [("--n") arg "Number of elements (default: 1000000)"
    (set! n (string->number arg))]
   [("--unique") arg "Number of unique values (default: 100000)"
    (set! num-unique (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)])

  ;; Generate input data
  (define data (generate-data-with-duplicates n num-unique seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'num-unique num-unique)
                       (list 'seed seed)
                       (list 'workers workers)))

  (printf "Running sequential dedup(n=~a, unique=~a)...\n" n num-unique)
  (define seq-result
    (run-benchmark
     (λ () (dedup-sequential data))
     #:name 'dedup
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel dedup(n=~a, unique=~a) (workers=~a)...\n" n num-unique workers)
  (define par-result
    (run-benchmark
     (λ () (dedup-parallel data workers))
     #:name 'dedup
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (printf "Sequential: ~a unique, Parallel: ~a unique\n"
          (vector-length seq-result)
          (vector-length par-result))

  (if (= (vector-length seq-result) (vector-length par-result))
      (printf "✓ Sequential and parallel counts match\n")
      (printf "✗ Counts differ!\n")))
