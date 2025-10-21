#lang racket/base

(require racket/fixnum
         racket/future
         (for-syntax racket/base))

(provide for/parallel
         call-with-thread-pool
         thread-pool-submit
         thread-pool-wait
         thread-pool-wait/collect
         thread-pool-for-each
         current-parallel-strategy
         set-parallel-strategy!)

;; Submit a thunk to a thread pool, keeping the result for retrieval.
(define (thread-pool-submit pool thunk)
  (unless (or (not pool) (parallel-thread-pool? pool))
    (error 'thread-pool-submit "invalid pool: ~a" pool))
  (thread #:pool pool #:keep 'results thunk))

;; Wait for a thread spawned via thread-pool-submit and return all produced values.
(define (thread-pool-wait t)
  (thread-wait t))

;; Wait for a sequence of threads and collect their results into a list.
(define (thread-pool-wait/collect threads)
  (for/list ([t (in-list threads)])
    (call-with-values
     (λ () (thread-pool-wait t))
     (λ results
       (cond
         [(null? results) (void)]
         [(null? (cdr results)) (car results)]
         [else results])))))

;; Apply a thunk-generating procedure across indices using a shared pool.
(define (thread-pool-for-each pool count proc)
  (define threads
    (for/list ([i (in-range count)])
      (thread-pool-submit pool (λ () (proc i)))))
  (for ([t (in-list threads)])
    (thread-pool-wait t)))

;; Parallel for loop implemented purely with thread pools.
(define-syntax (for/parallel stx)
  (syntax-case stx ()
    [(_ workers ([i N]) body ...)
     #'(let* ((total N)
              (integer-total? (exact-nonnegative-integer? total))
              (max-tasks (and integer-total? total))
              (worker-count (max 1 (min workers (if integer-total? total workers)))))
        (unless integer-total?
          (error 'for/parallel "expected exact nonnegative integer length, got ~a" total))
        (call-with-thread-pool worker-count
          (λ (pool actual-workers)
            (define total+ (max 1 total))
            (define effective-workers (max 1 (min actual-workers total+)))
            (define chunk-size (max 1 (ceiling (/ total+ effective-workers))))
            (define threads
              (for/list ([start (in-range 0 total chunk-size)])
                (let ((end (min total (+ start chunk-size))))
                  (thread-pool-submit
                   pool
                   (λ ()
                     (for ([i (in-range start end)])
                       body ...))))))
            (for ([t (in-list threads)])
              (thread-pool-wait t)))
          #:max max-tasks))]))
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
    (λ ()
      (proc pool count))
    (λ () (parallel-thread-pool-close pool))))

;; Track the currently requested parallel strategy for benchmarks.
(define current-parallel-strategy (make-parameter 'threads))

(define supported-strategies '(threads))

(define (set-parallel-strategy! sym)
  (define normalized
    (cond
      [(symbol? sym) sym]
      [(string? sym) (string->symbol (string-downcase sym))]
      [else (error 'set-parallel-strategy! "expected strategy symbol or string, got ~a" sym)]))
  (unless (member normalized supported-strategies)
    (error 'set-parallel-strategy!
           "unknown strategy ~a (supported strategies: ~a)"
           normalized
           supported-strategies))
  (current-parallel-strategy normalized))
