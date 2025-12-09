#lang racket

(require racket/fixnum
         racket/flonum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide spectral-norm)

;; Matrix element computation
(define (A i j)
  (define ij (fx+ i j))
  (fl/ 1.0 (fl+ (fl* (fl* (fx->fl ij) (fx->fl (fx+ ij 1))) 0.5)
                (fx->fl (fx+ i 1)))))

;; Apply A to vector x, store in y (sequential)
(define (Av-sequential x y N)
  (for ([i (in-range N)])
    (flvector-set!
     y i
     (for/fold ([a 0.0]) ([j (in-range N)])
       (fl+ a (fl* (flvector-ref x j) (A i j)))))))

;; Apply A to vector x, store in y (parallel)
(define (Av-parallel x y N workers)
  (define pool (make-parallel-thread-pool workers))
  (define chunk-size (quotient (+ N workers -1) workers))
  (define thds
    (for/list ([start (in-range 0 N chunk-size)])
      (define end (min N (+ start chunk-size)))
      (thread #:pool pool #:keep 'results
        (lambda ()
          (for ([i (in-range start end)])
            (flvector-set!
             y i
             (for/fold ([a 0.0]) ([j (in-range N)])
               (fl+ a (fl* (flvector-ref x j) (A i j))))))))))
  (for-each thread-wait thds)
  (parallel-thread-pool-close pool))

;; Apply A^T to vector x, store in y (sequential)
(define (Atv-sequential x y N)
  (for ([i (in-range N)])
    (flvector-set!
     y i
     (for/fold ([a 0.0]) ([j (in-range N)])
       (fl+ a (fl* (flvector-ref x j) (A j i)))))))

;; Apply A^T to vector x, store in y (parallel)
(define (Atv-parallel x y N workers)
  (define pool (make-parallel-thread-pool workers))
  (define chunk-size (quotient (+ N workers -1) workers))
  (define thds
    (for/list ([start (in-range 0 N chunk-size)])
      (define end (min N (+ start chunk-size)))
      (thread #:pool pool #:keep 'results
        (lambda ()
          (for ([i (in-range start end)])
            (flvector-set!
             y i
             (for/fold ([a 0.0]) ([j (in-range N)])
               (fl+ a (fl* (flvector-ref x j) (A j i))))))))))
  (for-each thread-wait thds)
  (parallel-thread-pool-close pool))

;; Apply A^T * A to vector x
(define (AtAv x y t N workers)
  (if (= workers 1)
      (begin
        (Av-sequential x t N)
        (Atv-sequential t y N))
      (begin
        (Av-parallel x t N workers)
        (Atv-parallel t y N workers))))

(define (spectral-norm n #:workers [workers 1] #:iterations [iterations 10])
  (define worker-count (max 1 workers))
  (define u (make-flvector n 1.0))
  (define v (make-flvector n))
  (define t (make-flvector n))
  (for ([iter iterations])
    (AtAv u v t n worker-count)
    (AtAv v u t n worker-count))
  (define vBv
    (for/fold ([sum 0.0]) ([i (in-range n)])
      (fl+ sum (fl* (flvector-ref u i) (flvector-ref v i)))))
  (define vv
    (for/fold ([sum 0.0]) ([i (in-range n)])
      (fl+ sum (fl* (flvector-ref v i) (flvector-ref v i)))))
  (flsqrt (fl/ vBv vv)))

(module+ main
  (define n 1000)
  (define workers (processor-count))
  (define iterations 10)
  (define repeat 1)
  (define log-path #f)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "spectral-norm.rkt"
    #:once-each
    [("--n") arg "Matrix dimension"
     (set! n (parse-positive-integer arg 'spectral-norm))]
    [("--iterations" "-i") arg "Power method iterations"
     (set! iterations (parse-positive-integer arg 'spectral-norm))]
    [("--workers") arg "Parallel thread count"
     (set! workers (parse-positive-integer arg 'spectral-norm))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'spectral-norm))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'iterations iterations)
                       (list 'workers workers)))

  (unless skip-sequential
    (run-benchmark
     (λ () (spectral-norm n #:workers 1 #:iterations iterations))
     #:name 'spectral-norm
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (spectral-norm n #:workers workers #:iterations iterations))
   #:name 'spectral-norm
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata)

  (close-log-writer writer))
