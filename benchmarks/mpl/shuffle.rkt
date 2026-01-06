#lang racket

;; Port of shuf from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/shuf
;; Adapted for Racket parallel benchmarking
;;
;; MPL uses bucket-based parallel shuffle:
;; 1. Hash each index to a bucket
;; 2. Use counting sort to redistribute elements by bucket
;; 3. Shuffle within each bucket in parallel (using ForkJoin.parfor)
;;
;; Key insight: No global in-place parallel shuffle exists without atomics,
;; but bucket partitioning allows independent parallel shuffles.

(require racket/fixnum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide shuffle-sequential
         shuffle-parallel
         verify-permutation)

;; Hash function for bucket assignment (matches MPL's Util.hash)
(define (hash-int seed i)
  (define x (bitwise-xor seed i))
  (define a #x5bd1e995)
  (define m (arithmetic-shift 1 32))
  (modulo (* (bitwise-xor x (arithmetic-shift x -13)) a) m))

;; Sequential in-place Fisher-Yates shuffle (Knuth shuffle)
;; This is the core algorithm used by both sequential and within each bucket
(define (shuffle-in-place! vec lo hi seed)
  (for ([i (in-range (- hi 1) lo -1)])
    (define j (+ lo (modulo (hash-int seed i) (+ 1 (- i lo)))))
    (define tmp (unsafe-vector-ref vec i))
    (unsafe-vector-set! vec i (unsafe-vector-ref vec j))
    (unsafe-vector-set! vec j tmp)))

;; Sequential Fisher-Yates shuffle
(define (shuffle-sequential vec seed)
  (define result (vector-copy vec))
  (define n (vector-length result))
  (shuffle-in-place! result 0 n seed)
  result)

;; Parallel bucket-based shuffle (MPL algorithm)
;;
;; Algorithm:
;; 1. Compute which bucket each element belongs to (hash-based)
;; 2. Count elements per bucket (parallel)
;; 3. Compute bucket offsets via prefix sum
;; 4. Redistribute elements to their buckets (parallel counting sort)
;; 5. Shuffle within each bucket in parallel
(define (shuffle-parallel vec workers seed [grain 5000])
  (define n (vector-length vec))

  (cond
    ;; Small input: use sequential
    [(< n 1000)
     (shuffle-sequential vec seed)]

    [else
     ;; Compute number of buckets based on input size (from MPL)
     ;; For n < 2^27: bits = (log2(n) - 7) / 2
     ;; For n >= 2^27: bits = log2(n) - 17
     (define log2-n (inexact->exact (ceiling (/ (log n) (log 2)))))
     (define bits (if (< n (expt 2 27))
                      (max 1 (quotient (- log2-n 7) 2))
                      (max 1 (- log2-n 17))))
     (define num-buckets (arithmetic-shift 1 bits))
     (define bucket-mask (- num-buckets 1))

     ;; Function to compute bucket for index i
     (define (bucket-of i)
       (bitwise-and (hash-int seed i) bucket-mask))

     ;; Phase 1: Count elements per bucket (parallel)
     (define pool (make-parallel-thread-pool workers))
     (define chunk-size (max 1 (quotient (+ n workers -1) workers)))

     ;; Each worker counts its chunk's bucket distribution
     (define count-channels
       (for/list ([w (in-range workers)])
         (define ch (make-channel))
         (define start (* w chunk-size))
         (define end (min (+ start chunk-size) n))
         (thread #:pool pool
           (λ ()
             (define local-counts (make-vector num-buckets 0))
             (for ([i (in-range start end)])
               (define b (bucket-of i))
               (unsafe-vector-set! local-counts b
                 (unsafe-fx+ 1 (unsafe-vector-ref local-counts b))))
             (channel-put ch local-counts)))
         ch))

     (define local-counts-list (map channel-get count-channels))

     ;; Merge counts
     (define bucket-counts (make-vector num-buckets 0))
     (for ([local-counts (in-list local-counts-list)])
       (for ([b (in-range num-buckets)])
         (unsafe-vector-set! bucket-counts b
           (unsafe-fx+ (unsafe-vector-ref bucket-counts b)
                       (unsafe-vector-ref local-counts b)))))

     ;; Phase 2: Compute bucket offsets (prefix sum)
     (define bucket-offsets (make-vector (+ num-buckets 1) 0))
     (for ([b (in-range num-buckets)])
       (unsafe-vector-set! bucket-offsets (+ b 1)
         (unsafe-fx+ (unsafe-vector-ref bucket-offsets b)
                     (unsafe-vector-ref bucket-counts b))))

     ;; Phase 3: Redistribute elements to buckets (counting sort)
     ;; Each worker needs its own write positions per bucket
     (define result (make-vector n))
     (define write-positions (vector-copy bucket-offsets))

     ;; Sequential redistribution (to avoid atomics)
     ;; MPL uses parallel counting sort, but we do sequential to avoid CAS
     (for ([i (in-range n)])
       (define b (bucket-of i))
       (define pos (unsafe-vector-ref write-positions b))
       (unsafe-vector-set! result pos (unsafe-vector-ref vec i))
       (unsafe-vector-set! write-positions b (unsafe-fx+ pos 1)))

     ;; Phase 4: Shuffle within each bucket in parallel
     (define shuffle-channels
       (for/list ([b (in-range num-buckets)])
         (define ch (make-channel))
         (define start (unsafe-vector-ref bucket-offsets b))
         (define end (unsafe-vector-ref bucket-offsets (+ b 1)))
         (if (> (- end start) 1)
             (begin
               (thread #:pool pool
                 (λ ()
                   (shuffle-in-place! result start end (+ seed b))
                   (channel-put ch #t)))
               ch)
             (begin
               (channel-put ch #t)
               ch))))

     (for-each channel-get shuffle-channels)
     (parallel-thread-pool-close pool)
     result]))

;; Verify that result is a permutation of original
(define (verify-permutation original shuffled)
  (define n (vector-length original))
  (and (= n (vector-length shuffled))
       ;; Check: same elements (sorted order)
       (let ([orig-sorted (vector-sort (vector-copy original) <)]
             [shuf-sorted (vector-sort (vector-copy shuffled) <)])
         (for/and ([i (in-range n)])
           (= (vector-ref orig-sorted i) (vector-ref shuf-sorted i))))))

;; Check that shuffle actually changed something
(define (different? v1 v2)
  (define n (vector-length v1))
  (for/or ([i (in-range n)])
    (not (= (vector-ref v1 i) (vector-ref v2 i)))))

;; Generate input vector [0, 1, 2, ..., n-1]
(define (make-input-vector n)
  (for/vector ([i (in-range n)]) i))

(module+ main
  (define n 1000000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define chunk-size 10000)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "shuffle.rkt"
    #:once-each
    [("--n") arg "Vector size"
     (set! n (parse-positive-integer arg 'shuffle))]
    [("--chunk-size") arg "Chunk size for parallel computation"
     (set! chunk-size (parse-positive-integer arg 'shuffle))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'shuffle))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'shuffle))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'shuffle))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (printf "Generating input vector (n=~a)...\n" n)
  (define input (make-input-vector n))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'chunk-size chunk-size)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential shuffle...\n")
    (set! seq-result
      (run-benchmark
       (λ () (shuffle-sequential input seed))
       #:name 'shuffle
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel shuffle (workers=~a, chunk-size=~a)...\n"
          workers chunk-size)
  (define par-result
    (run-benchmark
     (λ () (shuffle-parallel input workers seed chunk-size))
     #:name 'shuffle
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (verify-permutation input result)))
                 (error 'shuffle "parallel result is not a valid permutation at iteration ~a"
                        iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification:\n")
    (printf "  Sequential is permutation: ~a\n"
            (if (verify-permutation input seq-result) "✓" "✗"))
    (printf "  Parallel is permutation: ~a\n"
            (if (verify-permutation input par-result) "✓" "✗"))
    (printf "  Sequential shuffled: ~a\n"
            (if (different? input seq-result) "✓" "✗"))
    (printf "  Parallel shuffled: ~a\n"
            (if (different? input par-result) "✓" "✗")))

  (when (<= n 20)
    (printf "\nOriginal: ~a\n" input)
    (when seq-result
      (printf "Sequential: ~a\n" seq-result))
    (printf "Parallel: ~a\n" par-result)))
