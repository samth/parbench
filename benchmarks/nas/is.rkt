#lang racket

(require racket/fixnum
         "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt"
         "../common/run.rkt"
         "common/classes.rkt")

(provide is
         is-result-keys
         is-result-stats
         (struct-out is-result)
         (struct-out is-stats))

;; IS benchmark sorts random keys using bucket sort
;; Based on NAS Parallel Benchmarks IS specification

(struct is-result (keys stats) #:transparent)
(struct is-stats (num-keys key-range test-rank-min test-rank-max) #:transparent)

;; NAS IS parameters per class
(define is-class->params
  (hash 'S (hash 'total-keys (expt 2 16)
                 'max-key (expt 2 11)
                 'num-buckets 1024
                 'test-iterations 10)
        'W (hash 'total-keys (expt 2 20)
                 'max-key (expt 2 16)
                 'num-buckets 1024
                 'test-iterations 10)
        'A (hash 'total-keys (expt 2 23)
                 'max-key (expt 2 19)
                 'num-buckets 1024
                 'test-iterations 10)
        'B (hash 'total-keys (expt 2 25)
                 'max-key (expt 2 21)
                 'num-buckets 1024
                 'test-iterations 10)
        'C (hash 'total-keys (expt 2 27)
                 'max-key (expt 2 23)
                 'num-buckets 1024
                 'test-iterations 10)))

;; LCG parameters matching NAS specification
(define modulus (expt 2 46))
(define multiplier 1220703125)
(define base-seed 314159265)

(define (lcg-next seed)
  (modulo (* seed multiplier) modulus))

(define (mod-pow base exp mod)
  (let loop ([result 1]
             [base (modulo base mod)]
             [exp exp])
    (cond
      [(zero? exp) result]
      [(even? exp)
       (loop result (modulo (* base base) mod) (quotient exp 2))]
      [else
       (loop (modulo (* result base) mod)
             (modulo (* base base) mod)
             (quotient (sub1 exp) 2))])))

(define (lcg-advance seed steps)
  (if (zero? steps)
      seed
      (modulo (* seed (mod-pow multiplier steps modulus)) modulus)))

;; Generate keys for a specific range
(define (generate-keys start count max-key)
  (define seed (lcg-advance base-seed start))
  (define keys (make-vector count))
  (let loop ([i 0] [s seed])
    (when (< i count)
      (define next-seed (lcg-next s))
      (define key (modulo next-seed max-key))
      (vector-set! keys i key)
      (loop (add1 i) next-seed)))
  keys)

;; Sequential bucket sort
(define (is-sequential-sort keys max-key num-buckets)
  (define n (vector-length keys))
  (define bucket-size (quotient (+ max-key num-buckets -1) num-buckets))

  ;; Count keys per bucket
  (define bucket-counts (make-vector num-buckets 0))
  (for ([key (in-vector keys)])
    (define bucket-id (quotient key bucket-size))
    (when (>= bucket-id num-buckets)
      (set! bucket-id (sub1 num-buckets)))
    (vector-set! bucket-counts bucket-id
                 (fx+ 1 (vector-ref bucket-counts bucket-id))))

  ;; Compute prefix sums (bucket start positions)
  (define bucket-ptrs (make-vector num-buckets 0))
  (for ([i (in-range 1 num-buckets)])
    (vector-set! bucket-ptrs i
                 (fx+ (vector-ref bucket-ptrs (fx- i 1))
                      (vector-ref bucket-counts (fx- i 1)))))

  ;; Distribute keys to buckets
  (define sorted-keys (make-vector n 0))
  (define write-ptrs (vector-copy bucket-ptrs))

  (for ([key (in-vector keys)])
    (define bucket-id (quotient key bucket-size))
    (when (>= bucket-id num-buckets)
      (set! bucket-id (sub1 num-buckets)))
    (define pos (vector-ref write-ptrs bucket-id))
    (vector-set! sorted-keys pos key)
    (vector-set! write-ptrs bucket-id (fx+ pos 1)))

  ;; Sort within each bucket
  (for ([bucket (in-range num-buckets)])
    (define start (vector-ref bucket-ptrs bucket))
    (define end (if (< bucket (sub1 num-buckets))
                    (vector-ref bucket-ptrs (add1 bucket))
                    n))
    (when (< start end)
      (define subvec (vector-copy sorted-keys start end))
      (vector-sort! subvec <)
      (vector-copy! sorted-keys start subvec)))

  sorted-keys)

;; Parallel bucket sort
(define (is-parallel-sort keys max-key num-buckets workers)
  (define n (vector-length keys))
  (define bucket-size (quotient (+ max-key num-buckets -1) num-buckets))

  (call-with-thread-pool workers
    (λ (pool actual-workers)
      (define chunk-size (quotient (+ n actual-workers -1) actual-workers))

      ;; Step 1: Parallel local bucket counting
      (define local-counts-list
        (thread-pool-wait/collect
         (for/list ([w (in-range actual-workers)])
           (define start (* w chunk-size))
           (define end (min (+ start chunk-size) n))
           (thread-pool-submit
            pool
            (λ ()
              (define local-counts (make-vector num-buckets 0))
              (for ([i (in-range start end)])
                (define key (vector-ref keys i))
                (define bucket-id (quotient key bucket-size))
                (when (>= bucket-id num-buckets)
                  (set! bucket-id (sub1 num-buckets)))
                (vector-set! local-counts bucket-id
                             (fx+ 1 (vector-ref local-counts bucket-id))))
              local-counts)))))

      ;; Step 2: Merge local counts to get global bucket counts
      (define bucket-counts (make-vector num-buckets 0))
      (for ([local-counts (in-list local-counts-list)])
        (for ([bucket (in-range num-buckets)])
          (vector-set! bucket-counts bucket
                       (fx+ (vector-ref bucket-counts bucket)
                            (vector-ref local-counts bucket)))))

      ;; Step 3: Compute prefix sums
      (define bucket-ptrs (make-vector num-buckets 0))
      (for ([i (in-range 1 num-buckets)])
        (vector-set! bucket-ptrs i
                     (fx+ (vector-ref bucket-ptrs (fx- i 1))
                          (vector-ref bucket-counts (fx- i 1)))))

      ;; Step 4: Distribute keys (sequential for correctness)
      (define sorted-keys (make-vector n 0))
      (define write-ptrs (vector-copy bucket-ptrs))

      (for ([key (in-vector keys)])
        (define bucket-id (quotient key bucket-size))
        (when (>= bucket-id num-buckets)
          (set! bucket-id (sub1 num-buckets)))
        (define pos (vector-ref write-ptrs bucket-id))
        (vector-set! sorted-keys pos key)
        (vector-set! write-ptrs bucket-id (fx+ pos 1)))

      ;; Step 5: Parallel sort within buckets
      (define sort-threads
        (for/list ([bucket (in-range num-buckets)])
          (define start (vector-ref bucket-ptrs bucket))
          (define end (if (< bucket (sub1 num-buckets))
                          (vector-ref bucket-ptrs (add1 bucket))
                          n))
          (and (< start end)
               (thread-pool-submit
                pool
                (λ ()
                  (define subvec (vector-copy sorted-keys start end))
                  (vector-sort! subvec <)
                  (cons start subvec))))))

      ;; Copy sorted sub-buckets back
      (for ([task (in-list sort-threads)])
        (when task
          (define result (thread-pool-wait task))
          (define start (car result))
          (define subvec (cdr result))
          (vector-copy! sorted-keys start subvec)))

      sorted-keys)
    #:max (if (zero? n) 1 n)))

;; Verify that the result is sorted
(define (verify-sorted keys)
  (for/and ([i (in-range 1 (vector-length keys))])
    (<= (vector-ref keys (fx- i 1))
        (vector-ref keys i))))

;; Main IS benchmark function
(define (is #:class [class default-nas-class]
            #:workers [workers 1]
            #:total-keys [total-keys-override #f])
  (define class* (ensure-class class))
  (define params (hash-ref is-class->params class*
                           (λ () (error 'is "no IS parameters for class ~a" class*))))
  (define total-keys (or total-keys-override (hash-ref params 'total-keys)))
  (define max-key (hash-ref params 'max-key))
  (define num-buckets (hash-ref params 'num-buckets))

  ;; Generate keys
  (define keys (generate-keys 0 total-keys max-key))

  ;; Sort keys
  (define sorted-keys
    (cond
      [(<= workers 1) (is-sequential-sort keys max-key num-buckets)]
      [else (is-parallel-sort keys max-key num-buckets workers)]))

  ;; Verify and return
  (unless (verify-sorted sorted-keys)
    (error 'is "result not sorted"))

  (define stats (is-stats total-keys max-key 0 (sub1 total-keys)))
  (is-result sorted-keys stats))

(define (ensure-class sym)
  (define class (string->symbol (string-upcase (symbol->string sym))))
  (unless (hash-has-key? is-class->params class)
    (error 'is "unknown NAS class ~a" sym))
  class)

(module+ main
  (define class default-nas-class)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define total-keys-override #f)

  (void
   (command-line
    #:program "is.rkt"
    #:once-each
    [("--class") arg "NAS problem class (S, W, A, B, C)"
     (set! class (string->symbol (string-upcase arg)))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'nas-is))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'nas-is))]
    [("--total-keys") arg "Override total keys (testing)"
     (set! total-keys-override (string->number arg))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define actual-class (ensure-class class))
  (define params (hash-ref is-class->params actual-class))
  (define total-keys (or total-keys-override (hash-ref params 'total-keys)))
  (define max-key (hash-ref params 'max-key))
  (define num-buckets (hash-ref params 'num-buckets))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define bench-params (list (list 'class actual-class)
                             (list 'total-keys total-keys)
                             (list 'max-key max-key)
                             (list 'num-buckets num-buckets)
                             (list 'workers workers)))

  (printf "Running NAS IS benchmark: class=~a, total-keys=~a, max-key=~a\n"
          actual-class total-keys max-key)

  (define sequential-result
    (run-benchmark
     (λ () (is #:class actual-class #:total-keys total-keys #:workers 1))
     #:name 'nas-is
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params bench-params
     #:metadata metadata))

  (define _parallel-result
    (run-benchmark
     (λ () (is #:class actual-class #:total-keys total-keys #:workers workers))
     #:name 'nas-is
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params bench-params
     #:metadata metadata
     #:check (λ (_ value)
               (unless (equal? (is-result-keys value)
                              (is-result-keys sequential-result))
                 (error 'nas-is "parallel mismatch")))))

  (close-log-writer writer)

  (printf "\nBenchmark completed successfully.\n")
  (printf "Keys sorted: ~a\n" total-keys)
  (printf "Sample keys (first 10): ~a\n"
          (for/list ([i (in-range (min 10 total-keys))])
            (vector-ref (is-result-keys sequential-result) i))))
