#lang racket/base

(require racket/future
         racket/fixnum
         (for-syntax racket/base))

(provide for/parallel
         call-with-thread-pool
         thread-pool-submit
         thread-pool-wait
         thread-pool-wait/collect
         thread-pool-for-each)

;; Submit a thunk to a thread pool, keeping the result for retrieval.
(define (thread-pool-submit pool thunk)
  (thread #:pool pool #:keep 'results thunk))

;; Wait for a thread spawned via thread-pool-submit and return all produced values.
(define (thread-pool-wait t)
  (thread-wait t))

;; Wait for a sequence of threads and collect their results into a list.
(define (thread-pool-wait/collect threads)
  (for/list ([t (in-list threads)])
    (call-with-values
     (λ () (thread-pool-wait t))
     list)))

;; Apply a thunk-generating procedure across indices using a shared pool.
(define (thread-pool-for-each pool count proc)
  (define threads
    (for/list ([i (in-range count)])
      (thread-pool-submit pool (λ () (proc i)))))
  (for ([t (in-list threads)])
    (thread-pool-wait t)))

;; Parallel for loop implemented purely with thread pools.
(define-syntax for/parallel
  (syntax-rules ()
    [(_ workers ([i N]) body ...)
     (let* ([total N]
            [worker-count (max 1 (min workers (if (exact-nonnegative-integer? total)
                                                 total
                                                 workers)))]
            [pool (make-parallel-thread-pool worker-count)]
            [stride (if (zero? worker-count)
                        total
                        (fxquotient total worker-count))])
       (dynamic-wind
         (λ () (void))
         (λ ()
           (define threads
             (for/list ([n (in-range worker-count)])
               (define start (fx* n stride))
               (define end (fxmin total (fx* (fx+ n 1) stride)))
               (thread-pool-submit
                pool
                (λ ()
                  (for ([i (in-range start end)])
                    body ...)))))
           (for ([t (in-list threads)]) (thread-pool-wait t)))
         (λ () (parallel-thread-pool-close pool))))]))
;; Create a temporary thread pool, ensure it is closed, and run proc with it.
(define (call-with-thread-pool workers proc #:max [max-tasks #f])
  (define target (max 1 workers))
  (define count (cond
                  [(and max-tasks max-tasks)
                   (max 1 (min target max-tasks))]
                  [else target]))
  (define pool (make-parallel-thread-pool count))
  (dynamic-wind
    (λ () (void))
    (λ () (proc pool count))
    (λ () (parallel-thread-pool-close pool))))
