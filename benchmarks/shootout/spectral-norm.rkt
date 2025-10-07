#lang racket

(require racket/fixnum
         racket/flonum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide spectral-norm)

;; Matrix element computation
(define (A i j)
  (define ij (fx+ i j))
  (fl/ 1.0 (fl+ (fl* (fl* (fx->fl ij) (fx->fl (fx+ ij 1))) 0.5)
                (fx->fl (fx+ i 1)))))

;; Apply A to vector x, store in y
(define (Av x y N workers)
  (for/parallel workers ([i N])
    (flvector-set!
     y i
     (for/fold ([a 0.0]) ([j (in-range N)])
       (fl+ a (fl* (flvector-ref x j) (A i j)))))))

;; Apply A^T to vector x, store in y
(define (Atv x y N workers)
  (for/parallel workers ([i N])
    (flvector-set!
     y i
     (for/fold ([a 0.0]) ([j (in-range N)])
       (fl+ a (fl* (flvector-ref x j) (A j i)))))))

;; Apply A^T * A to vector x
(define (AtAv x y t N workers)
  (Av x t N workers)
  (Atv t y N workers))

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
  (define strategy 'threads)

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
    [("--strategy") arg "Parallel strategy: threads or futures"
     (set! strategy (string->symbol arg))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (set-parallel-strategy! strategy)

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'iterations iterations)
                       (list 'workers workers)
                       (list 'strategy strategy)))

  (define sequential-value
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
   #:variant (string->symbol (format "parallel-~a" strategy))
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (λ (_ value)
             (define diff (abs (- value sequential-value)))
             (when (> diff 1e-6)
               (error 'spectral-norm "parallel mismatch"))))

  (close-log-writer writer))
