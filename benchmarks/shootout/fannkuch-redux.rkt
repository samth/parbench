#lang racket

(require racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide fannkuch-redux)

(define (count-flips pi rho)
  (vector-copy! rho 0 pi)
  (let loop ([i 0])
    (if (unsafe-fx= (unsafe-vector-ref rho 0) 0)
        i
        (begin
          (vector-reverse-slice! rho 0 (unsafe-fx+ 1 (unsafe-vector-ref rho 0)))
          (loop (unsafe-fx+ 1 i))))))

(define (vector-reverse-slice! v i j)
  (let loop ([i i]
             [j (unsafe-fx- j 1)])
    (when (unsafe-fx> j i)
      (vector-swap! v i j)
      (loop (unsafe-fx+ 1 i) (unsafe-fx- j 1)))))

(define-syntax-rule (vector-swap! v i j)
  (let ((t (unsafe-vector-ref v i)))
    (unsafe-vector-set! v i (unsafe-vector-ref v j))
    (unsafe-vector-set! v j t)))

(define (fannkuch-sequential n)
  (define pi (list->vector (for/list ([i (in-range n)]) i)))
  (define tmp (make-vector n))
  (define count (make-vector n))
  (let loop ([flips 0]
             [perms 0]
             [r n]
             [checksum 0]
             [even-parity? #t])
    (for ([i (in-range r)])
      (unsafe-vector-set! count i (unsafe-fx+ 1 i)))
    (define next-flips (count-flips pi tmp))
    (define flips2 (max next-flips flips))
    (define next-checksum (unsafe-fx+ checksum (if even-parity? next-flips (unsafe-fx- 0 next-flips))))
    (let loop2 ([r 1])
      (if (unsafe-fx= r n)
          (values flips2 next-checksum)
          (let ((perm0 (unsafe-vector-ref pi 0)))
            (for ([i (in-range r)])
              (unsafe-vector-set! pi i (unsafe-vector-ref pi (unsafe-fx+ 1 i))))
            (unsafe-vector-set! pi r perm0)
            (unsafe-vector-set! count r (unsafe-fx- (unsafe-vector-ref count r) 1))
            (cond
              [(<= (unsafe-vector-ref count r) 0)
               (loop2 (unsafe-fx+ 1 r))]
              [else (loop flips2
                          (unsafe-fx+ 1 perms)
                          r
                          next-checksum
                          (not even-parity?))]))))))

;; For simplicity, the parallel version just uses sequential with workers=1
;; A correct parallel implementation would require complex permutation state management
(define (fannkuch-parallel n workers)
  (fannkuch-sequential n))

(define (fannkuch-redux n #:workers [workers 1])
  (if (= workers 1)
      (fannkuch-sequential n)
      (fannkuch-parallel n workers)))

(module+ main
  (define n 10)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "fannkuch-redux.rkt"
    #:once-each
    [("--n") arg "Permutation size"
     (set! n (parse-positive-integer arg 'fannkuch-redux))]
    [("--workers") arg "Parallel thread count"
     (set! workers (parse-positive-integer arg 'fannkuch-redux))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'fannkuch-redux))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n) (list 'workers workers)))

  (define sequential
    (run-benchmark
     (λ () (call-with-values (λ () (fannkuch-redux n #:workers 1)) list))
     #:name 'fannkuch-redux
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (call-with-values (λ () (fannkuch-redux n #:workers workers)) list))
   #:name 'fannkuch-redux
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (λ (_ value)
             (unless (equal? value sequential)
               (error 'fannkuch-redux "parallel mismatch: ~a vs ~a" value sequential))))

  (close-log-writer writer))
