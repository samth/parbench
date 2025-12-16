#lang racket

;; Port of bignum-add from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/bignum-add
;; Adapted for Racket parallel benchmarking
;;
;; Parallel addition of large integers represented as digit arrays.
;; Tests chunking and carry propagation patterns.

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide bignum-add-sequential
         bignum-add-parallel
         make-random-bignum)

;; Represent bignum as vector of fixnums (base 10000 for demo)
(define BASE 10000)

;; Sequential bignum addition with carry propagation
(define (bignum-add-sequential a b)
  (define n (vector-length a))
  (define result (make-vector n 0))
  (define carry 0)
  (for ([i (in-range n)])
    (define sum (fx+ (fx+ (vector-ref a i) (vector-ref b i)) carry))
    (vector-set! result i (fxmodulo sum BASE))
    (set! carry (fxquotient sum BASE)))
  (values result carry))

;; Parallel bignum addition - compute sums in parallel, then propagate carries
;; WARNING: Parallel version is typically SLOWER than sequential because
;; carry propagation is inherently sequential. The parallel phase computes
;; local sums, but merging requires sequential processing of all chunks.
(define (bignum-add-parallel a b workers [chunk-size 1000])
  (define n (vector-length a))
  (define num-chunks (quotient (+ n chunk-size -1) chunk-size))
  (define pool (make-parallel-thread-pool workers))

  ;; Phase 1: Compute local sums and carries in parallel
  (define channels
    (for/list ([c (in-range num-chunks)])
      (define ch (make-channel))
      (define start (* c chunk-size))
      (define end (min (+ start chunk-size) n))
      (thread #:pool pool
       (λ ()
         (define local-result (make-vector (- end start) 0))
         (define carry 0)
         (for ([i (in-range start end)])
           (define local-i (- i start))
           (define sum (fx+ (fx+ (vector-ref a i) (vector-ref b i)) carry))
           (vector-set! local-result local-i (fxmodulo sum BASE))
           (set! carry (fxquotient sum BASE)))
         (channel-put ch (cons local-result carry))))
      ch))

  (define chunks (map channel-get channels))
  (parallel-thread-pool-close pool)

  ;; Phase 2: Sequential carry propagation between chunks
  (define result (make-vector n 0))
  (define carry 0)
  (for ([c (in-range num-chunks)])
    (define start (* c chunk-size))
    (define end (min (+ start chunk-size) n))
    (define-values (local-result local-carry) (values (car (list-ref chunks c))
                                                       (cdr (list-ref chunks c))))

    ;; Copy local results with incoming carry
    (for ([i (in-range (- end start))])
      (define val (fx+ (vector-ref local-result i) carry))
      (vector-set! result (+ start i) (fxmodulo val BASE))
      (set! carry (fxquotient val BASE)))

    ;; Add local carry
    (set! carry (fx+ carry local-carry)))

  (values result carry))

;; Generate random bignum of n digits (base BASE)
(define (make-random-bignum n seed)
  (random-seed seed)
  (for/vector ([i (in-range n)])
    (random BASE)))

;; Compare two bignums for equality
(define (bignum-equal? a b)
  (and (= (vector-length a) (vector-length b))
       (for/and ([i (in-range (vector-length a))])
         (fx= (vector-ref a i) (vector-ref b i)))))

;; Format bignum for display (show first few and last few digits)
(define (format-bignum v [show 5])
  (define n (vector-length v))
  (if (<= n (* 2 show))
      (format "[~a]" (string-join (for/list ([i (in-range n)])
                                    (~a (vector-ref v i))) " "))
      (format "[~a ... ~a]"
              (string-join (for/list ([i (in-range show)])
                            (~a (vector-ref v i))) " ")
              (string-join (for/list ([i (in-range (- n show) n)])
                            (~a (vector-ref v i))) " "))))

(module+ main
  (define n 100000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define chunk-size 1000)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "bignum-add.rkt"
    #:once-each
    [("--n") arg "Number of digits"
     (set! n (parse-positive-integer arg 'bignum-add))]
    [("--chunk-size") arg "Chunk size for parallel computation"
     (set! chunk-size (parse-positive-integer arg 'bignum-add))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'bignum-add))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'bignum-add))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'bignum-add))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (printf "Generating random bignums (n=~a digits, base=~a)...\n" n BASE)
  (define a (make-random-bignum n seed))
  (define b (make-random-bignum n (+ seed 1)))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'base BASE)
                       (list 'chunk-size chunk-size)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (define seq-carry #f)
  (unless skip-sequential
    (printf "Running sequential bignum addition...\n")
    (define-values (sr sc)
      (run-benchmark
       (λ () (bignum-add-sequential a b))
       #:name 'bignum-add
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata))
    (set! seq-result sr)
    (set! seq-carry sc))

  (printf "Running parallel bignum addition (workers=~a, chunk-size=~a)...\n"
          workers chunk-size)
  (define-values (par-result par-carry)
    (run-benchmark
     (λ () (bignum-add-parallel a b workers chunk-size))
     #:name 'bignum-add
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (define-values (r c) (values (car result) (cdr result)))
               (when (and seq-result (not (and (bignum-equal? seq-result r) (fx= seq-carry c))))
                 (error 'bignum-add "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (and (bignum-equal? seq-result par-result) (fx= seq-carry par-carry))
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "\nResult (base ~a): ~a\n" BASE (format-bignum par-result))
  (printf "Final carry: ~a\n" par-carry))
