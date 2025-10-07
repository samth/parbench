#lang racket/base

(require racket/future
         racket/fixnum
         (for-syntax racket/base))

(provide for/parallel
         parallel-strategy
         set-parallel-strategy!)

;; Current parallel strategy: 'futures or 'threads
(define parallel-strategy (make-parameter 'threads))

;; Set the parallelism strategy globally
(define (set-parallel-strategy! strategy)
  (unless (memq strategy '(futures threads))
    (error 'set-parallel-strategy! "unknown strategy: ~a" strategy))
  (parallel-strategy strategy))

;; Parallel for loop that dispatches based on strategy
(define-syntax for/parallel
  (syntax-rules ()
    [(_ workers ([i N]) body ...)
     (case (parallel-strategy)
       [(futures)
        (for/parallel-futures workers ([i N]) body ...)]
       [(threads)
        (for/parallel-threads workers ([i N]) body ...)])]))

;; Futures-based parallel for
(define-syntax for/parallel-futures
  (syntax-rules ()
    [(_ workers ([i N]) body ...)
     (let ([stride (fxquotient N workers)])
       (define fs
         (for/list ([n workers])
           (future (λ () (for ([i (in-range (fx* n stride)
                                             (fxmin N (fx* (fx+ n 1) stride)))])
                           body ...)))))
       (for-each touch fs))]))

;; Parallel threads-based parallel for
(define-syntax for/parallel-threads
  (syntax-rules ()
    [(_ workers ([i N]) body ...)
     (let ([pool (make-parallel-thread-pool workers)]
           [stride (fxquotient N workers)])
       (define threads
         (for/list ([n workers])
           (thread
            #:pool pool
            (λ () (for ([i (in-range (fx* n stride)
                                      (fxmin N (fx* (fx+ n 1) stride)))])
                    body ...)))))
       (for ([t threads]) (thread-wait t)))]))
