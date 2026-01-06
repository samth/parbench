#lang racket

;; Port of collect from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/collect
;; Adapted for Racket parallel benchmarking
;;
;; MPL's SeqBasis.filter algorithm:
;; 1. Phase 1: Parallel count of matches per chunk (using tabulate)
;; 2. Phase 2: Parallel prefix scan to compute write offsets
;; 3. Phase 3: Parallel write filtered elements to output array (using parfor)
;;
;; Key insight: Avoid list allocation - use pre-allocated vectors throughout.

(require racket/fixnum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide collect-sequential
         collect-parallel
         generate-input-vector)

;; Sequential collect - filter elements matching predicate
;; Uses two passes to avoid list allocation:
;; Pass 1: Count matches
;; Pass 2: Copy matches to pre-allocated result
(define (collect-sequential vec pred)
  (define n (vector-length vec))
  ;; Pass 1: Count matches
  (define count 0)
  (for ([i (in-range n)])
    (when (pred (unsafe-vector-ref vec i))
      (set! count (fx+ count 1))))
  ;; Pass 2: Copy matches
  (define result (make-vector count))
  (define pos 0)
  (for ([i (in-range n)])
    (define x (unsafe-vector-ref vec i))
    (when (pred x)
      (unsafe-vector-set! result pos x)
      (set! pos (fx+ pos 1))))
  result)

;; Parallel collect using MPL's three-phase approach
;; Optimized to avoid O(n) flags allocation by recomputing predicate in phase 3
(define (collect-parallel vec pred workers)
  (define n (vector-length vec))
  (define chunk-size (max 1 (quotient (+ n workers -1) workers)))
  (define num-chunks (quotient (+ n chunk-size -1) chunk-size))
  (define pool (make-parallel-thread-pool workers))

  ;; Phase 1: Parallel count of matches per chunk
  (define counts (make-vector num-chunks 0))

  (define count-channels
    (for/list ([c (in-range num-chunks)])
      (define ch (make-channel))
      (define start (fx* c chunk-size))
      (define end (min (fx+ start chunk-size) n))
      (thread #:pool pool
        (λ ()
          (define local-count 0)
          (for ([i (in-range start end)])
            (when (pred (unsafe-vector-ref vec i))
              (set! local-count (fx+ local-count 1))))
          (unsafe-vector-set! counts c local-count)
          (channel-put ch #t)))
      ch))

  (for-each channel-get count-channels)

  ;; Phase 2: Prefix sum to compute write offsets (sequential - small)
  (define offsets (make-vector (fx+ num-chunks 1) 0))
  (for ([c (in-range num-chunks)])
    (unsafe-vector-set! offsets (fx+ c 1)
      (fx+ (unsafe-vector-ref offsets c)
           (unsafe-vector-ref counts c))))

  (define total-count (unsafe-vector-ref offsets num-chunks))
  (define result (make-vector total-count))

  ;; Phase 3: Parallel write to result using computed offsets
  ;; Recompute predicate to avoid flags vector allocation
  (define write-channels
    (for/list ([c (in-range num-chunks)])
      (define ch (make-channel))
      (define start (fx* c chunk-size))
      (define end (min (fx+ start chunk-size) n))
      (define write-pos (unsafe-vector-ref offsets c))
      (thread #:pool pool
        (λ ()
          (define pos write-pos)
          (for ([i (in-range start end)])
            (define x (unsafe-vector-ref vec i))
            (when (pred x)
              (unsafe-vector-set! result pos x)
              (set! pos (fx+ pos 1))))
          (channel-put ch #t)))
      ch))

  (for-each channel-get write-channels)
  (parallel-thread-pool-close pool)

  result)

;; Generate input vector with mix of values
(define (generate-input-vector n seed)
  (random-seed seed)
  (for/vector ([i (in-range n)])
    (random 100)))

;; Verify two vectors are equal
(define (vectors-equal? v1 v2)
  (and (= (vector-length v1) (vector-length v2))
       (for/and ([i (in-range (vector-length v1))])
         (= (vector-ref v1 i) (vector-ref v2 i)))))

;; Example predicates
(define (even-pred x) (even? x))
(define (div-by-3-pred x) (zero? (modulo x 3)))
(define (greater-than-50-pred x) (> x 50))

(module+ main
  (define n 10000000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define predicate-name 'even)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "collect.rkt"
    #:once-each
    [("--n") arg "Vector size"
     (set! n (parse-positive-integer arg 'collect))]
    [("--predicate") arg "Predicate: even, div-by-3, gt-50"
     (set! predicate-name (string->symbol arg))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'collect))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'collect))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'collect))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (define pred
    (case predicate-name
      [(even) even-pred]
      [(div-by-3) div-by-3-pred]
      [(gt-50) greater-than-50-pred]
      [else (error 'collect "unknown predicate: ~a" predicate-name)]))

  (printf "Generating input vector (n=~a)...\n" n)
  (define input (generate-input-vector n seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'predicate predicate-name)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential collect (predicate: ~a)...\n" predicate-name)
    (set! seq-result
      (run-benchmark
       (λ () (collect-sequential input pred))
       #:name 'collect
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel collect (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (collect-parallel input pred workers))
     #:name 'collect
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (vectors-equal? seq-result result)))
                 (error 'collect "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (vectors-equal? seq-result par-result)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "Collected ~a elements (from ~a, ~a%)\n"
          (vector-length par-result)
          n
          (~r (* 100.0 (/ (vector-length par-result) n)) #:precision 1))

  (when (<= (vector-length par-result) 50)
    (printf "\nResult: ~a\n" par-result)))
