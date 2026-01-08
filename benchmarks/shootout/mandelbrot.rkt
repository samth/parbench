#lang racket

(require racket/flonum
         racket/fixnum
         (only-in racket/unsafe/ops
                  unsafe-fx+
                  unsafe-fx<
                  unsafe-fx=
                  unsafe-fx-
                  unsafe-fxlshift
                  unsafe-fxand)
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide mandelbrot)

(define LIMIT-SQR 4.0)
(define ITERATIONS 50)

(define-syntax-rule (mandelbrot-pixel Cr Ci)
  (let loop ([i 0] [Zr 0.0] [Zi 0.0])
    (cond [(fl> (fl+ (fl* Zr Zr) (fl* Zi Zi)) LIMIT-SQR) 0]
          [(unsafe-fx= i ITERATIONS) 1]
          [else (let ([Zr (fl+ (fl- (fl* Zr Zr) (fl* Zi Zi)) Cr)]
                      [Zi (fl+ (fl* 2.0 (fl* Zr Zi)) Ci)])
                  (loop (unsafe-fx+ i 1) Zr Zi))])))

(define (compute-row y N Crs)
  (define N.0 (fx->fl N))
  (define 2/N (fl/ 2.0 N.0))
  (define Ci (fl- (fl* 2/N (fx->fl y)) 1.0))
  (define bpr (ceiling (/ N 8)))
  (define row-bytes (make-bytes bpr))

  (let loop-x ([x 0] [bitnum 0] [byteacc 0] [byte-idx 0])
    (if (unsafe-fx< x N)
        (let* ([Cr (flvector-ref Crs x)]
               [bitnum (unsafe-fx+ bitnum 1)]
               [byteacc (unsafe-fx+ (unsafe-fxlshift byteacc 1) (mandelbrot-pixel Cr Ci))])
          (cond [(unsafe-fx= bitnum 8)
                 (bytes-set! row-bytes byte-idx byteacc)
                 (loop-x (unsafe-fx+ x 1) 0 0 (unsafe-fx+ byte-idx 1))]
                [else (loop-x (unsafe-fx+ x 1) bitnum byteacc byte-idx)]))
        (begin
          (when (> bitnum 0)
            (bytes-set! row-bytes byte-idx (unsafe-fxlshift byteacc (unsafe-fx- 8 (unsafe-fxand N #x7)))))
          row-bytes))))

(define (mandelbrot-sequential N)
  (define N.0 (fx->fl N))
  (define Crs
    (let ([v (make-flvector N)])
      (for ([x (in-range N)])
        (flvector-set! v x (fl- (fl/ (fx->fl (fx* 2 x)) N.0) 1.5)))
      v))

  (define bpr (ceiling (/ N 8)))
  (define bitmap (make-bytes (* N bpr)))

  (for ([y (in-range N)])
    (define row-bytes (compute-row y N Crs))
    (bytes-copy! bitmap (* y bpr) row-bytes))

  bitmap)

(define (mandelbrot-parallel N workers)
  (define N.0 (fx->fl N))
  (define Crs
    (let ([v (make-flvector N)])
      (for ([x (in-range N)])
        (flvector-set! v x (fl- (fl/ (fx->fl (fx* 2 x)) N.0) 1.5)))
      v))

  (define bpr (ceiling (/ N 8)))
  (define bitmap (make-bytes (* N bpr)))

  ;; Compute rows in parallel using thread pool
  (define pool (make-parallel-thread-pool workers))
  (define chunk-size (quotient (+ N workers -1) workers))
  (define thds
    (for/list ([start (in-range 0 N chunk-size)])
      (define end (min N (+ start chunk-size)))
      (thread #:pool pool #:keep 'results
        (lambda ()
          (for ([y (in-range start end)])
            (define row-bytes (compute-row y N Crs))
            (bytes-copy! bitmap (fx* y bpr) row-bytes))))))
  (for-each thread-wait thds)
  (parallel-thread-pool-close pool)

  bitmap)

(define (mandelbrot N #:workers [workers 1])
  (if (= workers 1)
      (mandelbrot-sequential N)
      (mandelbrot-parallel N workers)))

(module+ main
  (define N 200)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "mandelbrot.rkt"
    #:once-each
    [("--n") arg "Image size (NxN)"
     (set! N (parse-positive-integer arg 'mandelbrot))]
    [("--workers") arg "Parallel thread count"
     (set! workers (parse-positive-integer arg 'mandelbrot))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'mandelbrot))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'N N)
                       (list 'workers workers)))

  (unless skip-sequential
    (run-benchmark
     (λ () (mandelbrot N #:workers 1))
     #:name 'mandelbrot
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (mandelbrot N #:workers workers))
   #:name 'mandelbrot
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata)

  (close-log-writer writer))
