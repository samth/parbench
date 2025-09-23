#lang racket

(require racket/list
         "logging.rkt")

(provide run-benchmark)

(define (normalize-pairs items)
  (cond
    [(hash? items) (for/list ([(k v) (in-hash items)]) (list k v))]
    [(list? items)
     (for/list ([entry items])
       (cond
         [(and (list? entry) (>= (length entry) 2)) (list (first entry) (second entry))]
         [(pair? entry) (list (car entry) (cdr entry))]
         [else (list entry #t)]))]
    [else '()]))

(define (run-benchmark thunk
                       #:name name
                       #:variant variant
                       #:repeat [repeat 1]
                       #:log-writer [writer #f]
                       #:params [params '()]
                       #:metadata [metadata (system-metadata)]
                       #:check [check (λ (_ value) value)])
  (define normalized-params (normalize-pairs params))
  (define metadata-lists (normalize-pairs metadata))
  (define last-result #f)
  (for ([iteration (in-range repeat)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (define-values (results cpu real gc) (time-apply thunk '()))
    (define value (if (null? results) #f (car results)))
    (set! last-result value)
    (check iteration value)
    (define event
      `(benchmark
         (name ,name)
         (variant ,variant)
         (iteration ,(add1 iteration))
         (repeat ,repeat)
         (metrics (cpu-ms ,cpu) (real-ms ,real) (gc-ms ,gc))
         (params ,@(for/list ([p normalized-params]) `(,(first p) ,(second p))))
         (metadata ,@(for/list ([m metadata-lists]) `(,(first m) ,(second m))))
         (status ok)))
    (log-event writer event))
  last-result)
