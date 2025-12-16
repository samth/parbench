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
;; Sequential Counting Sort (using fxvectors throughout)
;; ============================================================================

(define (integer-sort-sequential data range)
  (define n (fxvector-length data))
  (define counts (make-fxvector range 0))

  ;; Count occurrences
  (for ([i (in-range n)])
    (define val (fxvector-ref data i))
    (fxvector-set! counts val (fx+ 1 (fxvector-ref counts val))))

  ;; Compute prefix sums (exclusive scan)
  (define positions (make-fxvector range 0))
  (for ([i (in-range 1 range)])
    (fxvector-set! positions i
                   (fx+ (fxvector-ref positions (fx- i 1))
                        (fxvector-ref counts (fx- i 1)))))

  ;; Place elements in sorted order
  (define result (make-fxvector n 0))
  (for ([i (in-range n)])
    (define val (fxvector-ref data i))
    (define pos (fxvector-ref positions val))
    (fxvector-set! result pos val)
    (fxvector-set! positions val (fx+ pos 1)))

  result)

;; ============================================================================
;; Parallel Counting Sort - Block-based approach (like MPL)
;; ============================================================================
;;
;; Algorithm (based on mpllib/CountingSort.sml):
;; 1. Divide input into blocks (one per worker)
;; 2. Each block counts occurrences independently (no cross-block reads)
;; 3. Compute global bucket offsets from block counts
;; 4. Each block scatters its elements to final positions using pre-computed offsets
;;
;; Key insight: By having each block read only its contiguous portion of input,
;; we avoid the memory bandwidth contention that caused parallel counting to be
;; slower than sequential with shared reads.
;;
;; Performance notes:
;; - The scatter phase (Step 4) DOES show speedup (1.19x with 4 workers)
;; - However, Racket's stop-the-world GC overhead with multiple workers
;;   often cancels out the gains (more allocated structures = more GC time)
;; - This is a fundamental limitation compared to MPL's disentangled GC
;; - Best results with smaller range values (range < 100000) where count
;;   vectors fit in cache

(define (integer-sort-parallel data range workers #:profile? [profile? #f])
  (define n (fxvector-length data))
  (define SEQ-THRESHOLD 8192)

  ;; Fall back to sequential for small inputs
  (when (< n SEQ-THRESHOLD)
    (integer-sort-sequential data range))

  (define num-blocks workers)
  (define block-size (quotient (+ n num-blocks -1) num-blocks))

  ;; Helper to optionally time a step
  (define-syntax-rule (timed-step name body)
    (if profile?
        (let-values ([(result-list cpu real gc) (time-apply (lambda () body) '())])
          (printf "    ~a: ~a ms\n" name real)
          (car result-list))
        body))

  (when profile?
    (printf "  Stage timings (workers=~a):\n" workers))

  ;; Pre-allocate per-block count fxvectors (unboxed fixnums = less GC pressure)
  (define block-counts (for/vector ([b (in-range num-blocks)])
                         (make-fxvector range 0)))

  ;; Create thread pool for true OS-level parallelism
  (define pool (make-parallel-thread-pool workers))

  ;; Step 1: Each block counts occurrences only (no local sort)
  (timed-step "Step 1 (block count)"
    (let ([threads
           (for/list ([b (in-range num-blocks)])
             (define start (* b block-size))
             (define end (min (+ start block-size) n))
             (define my-counts (vector-ref block-counts b))
             (if (>= start n)
                 #f
                 (thread #:pool pool
                  (lambda ()
                    ;; Count occurrences in this block
                    (for ([i (in-range start end)])
                      (define val (fxvector-ref data i))
                      (fxvector-set! my-counts val (fx+ 1 (fxvector-ref my-counts val))))))))])
      (for ([t threads] #:when t) (thread-wait t))))

  ;; Step 2: Compute global bucket offsets by summing counts across all blocks
  ;; (sequential - too fine-grained for parallelism)
  (define global-counts
    (timed-step "Step 2 (sum counts)"
      (let ([counts (make-fxvector range 0)])
        (for ([b (in-range num-blocks)])
          (define local-counts (vector-ref block-counts b))
          (for ([bucket (in-range range)])
            (fxvector-set! counts bucket
                           (fx+ (fxvector-ref counts bucket)
                                (fxvector-ref local-counts bucket)))))
        counts)))

  ;; Step 3: Compute global prefix sum and per-block offsets (sequential)
  (define block-offsets
    (for/vector ([b (in-range num-blocks)])
      (make-fxvector range 0)))
  (timed-step "Step 3 (prefix sum + offsets)"
    (let ([positions (make-fxvector range 0)])
      ;; Global prefix sum
      (for ([bucket (in-range 1 range)])
        (fxvector-set! positions bucket
                       (fx+ (fxvector-ref positions (fx- bucket 1))
                            (fxvector-ref global-counts (fx- bucket 1)))))
      ;; Compute per-block offsets for each bucket
      (for ([bucket (in-range range)])
        (define base (fxvector-ref positions bucket))
        (define running base)
        (for ([b (in-range num-blocks)])
          (fxvector-set! (vector-ref block-offsets b) bucket running)
          (set! running (fx+ running (fxvector-ref (vector-ref block-counts b) bucket)))))))

  ;; Step 4: Scatter elements from input to final positions
  ;; Each block scatters its elements using the pre-computed offsets
  (define result
    (timed-step "Step 4 (scatter)"
      (let ([res (make-fxvector n 0)])
        ;; Create per-block write positions (copy of offsets that we'll increment)
        (define write-positions
          (for/vector ([b (in-range num-blocks)])
            (fxvector-copy (vector-ref block-offsets b))))
        (let ([threads
               (for/list ([b (in-range num-blocks)])
                 (define start (* b block-size))
                 (define end (min (+ start block-size) n))
                 (define my-positions (vector-ref write-positions b))
                 (if (>= start n)
                     #f
                     (thread #:pool pool
                      (lambda ()
                        ;; Scatter this block's elements to final positions
                        (for ([i (in-range start end)])
                          (define val (fxvector-ref data i))
                          (define pos (fxvector-ref my-positions val))
                          (fxvector-set! res pos val)
                          (fxvector-set! my-positions val (fx+ pos 1)))))))])
          (for ([t threads] #:when t) (thread-wait t)))
        res)))

  (parallel-thread-pool-close pool)
  result)

;; ============================================================================
;; Utilities
;; ============================================================================

(define (generate-random-data n range seed)
  (random-seed seed)
  (define result (make-fxvector n 0))
  (for ([i (in-range n)])
    (fxvector-set! result i (random range)))
  result)

(define (verify-sorted data)
  (for/and ([i (in-range 1 (fxvector-length data))])
    (fx<= (fxvector-ref data (fx- i 1))
          (fxvector-ref data i))))

(define (fxvector->list fxv)
  (for/list ([i (in-range (fxvector-length fxv))])
    (fxvector-ref fxv i)))

(define (verify-same-elements data1 data2)
  (and (= (fxvector-length data1) (fxvector-length data2))
       (equal? (sort (fxvector->list data1) <)
               (sort (fxvector->list data2) <))))

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
  (define profile-mode #f)

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
     (set! skip-sequential #t)]
    [("--profile") "Run with internal timing breakdown"
     (set! profile-mode #t)]))

  (printf "Generating random data (n=~a, range=[0,~a))...\n" n range)
  (define data (generate-random-data n range seed))

  ;; If profile mode, run with timing and exit
  (when profile-mode
    (printf "\n=== Profile Mode ===\n")
    (printf "Running parallel integer sort with workers=1...\n")
    (void (integer-sort-parallel data range 1 #:profile? #t))
    (printf "\nRunning parallel integer sort with workers=4...\n")
    (void (integer-sort-parallel data range 4 #:profile? #t))
    (printf "\nDone profiling.\n")
    (exit 0))

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
    (printf "  result[~a] = ~a\n" i (fxvector-ref par-result i))))
