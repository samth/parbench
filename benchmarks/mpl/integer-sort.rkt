#lang racket

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide integer-sort-sequential
         integer-sort-parallel)

;; Sequential counting sort for bounded range
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

;; Parallel counting sort: parallel counting and scattering
(define (integer-sort-parallel data range workers)
  (define n (vector-length data))
  (define global-counts
    (call-with-thread-pool workers
      (λ (pool actual-workers)
        (define chunk-size (quotient (+ n actual-workers -1) actual-workers))
        ;; Step 1: Parallel counting - each worker counts its partition
        (define local-counts-list
          (thread-pool-wait/collect
           (for/list ([w (in-range actual-workers)])
             (define start (* w chunk-size))
             (define end (min (+ start chunk-size) n))
             (thread-pool-submit
              pool
              (λ ()
                (define local-counts (make-vector range 0))
                (for ([i (in-range start end)])
                  (define val (vector-ref data i))
                  (vector-set! local-counts val
                               (fx+ 1 (vector-ref local-counts val))))
                local-counts)))))
        ;; Step 2: Merge local counts
        (define merged (make-vector range 0))
        (for ([local-counts (in-list local-counts-list)])
          (for ([bucket (in-range range)])
            (vector-set! merged bucket
                         (fx+ (vector-ref merged bucket)
                              (vector-ref local-counts bucket)))))
        merged)
      #:max (if (zero? n) 1 n)))

  ;; Step 3: Compute prefix sums
  (define positions (make-vector range 0))
  (for ([i (in-range 1 range)])
    (vector-set! positions i
                 (fx+ (vector-ref positions (fx- i 1))
                      (vector-ref global-counts (fx- i 1)))))

  ;; Step 4: Parallel scatter - each worker writes its elements
  ;; Note: This requires careful synchronization or partitioning
  ;; For simplicity, we'll use a sequential scatter phase
  (define result (make-vector n 0))
  (define write-positions (vector-copy positions))

  (for ([val (in-vector data)])
    (define pos (vector-ref write-positions val))
    (vector-set! result pos val)
    (vector-set! write-positions val (fx+ pos 1)))

  result)

;; Generate random test data
(define (generate-random-data n range seed)
  (random-seed seed)
  (for/vector ([i (in-range n)])
    (random range)))

;; Verify that array is sorted
(define (verify-sorted data)
  (for/and ([i (in-range 1 (vector-length data))])
    (<= (vector-ref data (fx- i 1))
        (vector-ref data i))))

;; Verify that two arrays contain the same elements (permutation)
(define (verify-same-elements data1 data2)
  (and (= (vector-length data1) (vector-length data2))
       (equal? (vector->list (vector-sort data1 <))
               (vector->list (vector-sort data2 <)))))

(module+ main
  (define n 10000000)
  (define range 1000000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)

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
     (set! log-path arg)]))

  (printf "Generating random data (n=~a, range=[0,~a))...\n" n range)
  (define data (generate-random-data n range seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'range range)
                       (list 'workers workers)
                       (list 'seed seed)))

  (printf "Running sequential integer sort...\n")
  (define seq-result
    (run-benchmark
     (λ () (integer-sort-sequential data range))
     #:name 'integer-sort
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel integer sort (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (integer-sort-parallel data range workers))
     #:name 'integer-sort
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (and (verify-sorted result)
                           (verify-same-elements data result))
                 (error 'integer-sort "parallel result invalid at iteration ~a" iteration)))))

  (close-log-writer writer)

  (printf "\nVerification:\n")
  (printf "  Sequential sorted: ~a\n" (verify-sorted seq-result))
  (printf "  Parallel sorted: ~a\n" (verify-sorted par-result))
  (printf "  Same elements: ~a\n" (verify-same-elements seq-result par-result))

  (printf "\nSample values (first 10):\n")
  (for ([i (in-range (min 10 n))])
    (printf "  result[~a] = ~a\n" i (vector-ref seq-result i))))
