#lang racket

(require racket/list
         racket/math
         racket/format)

(provide load-summaries
         summary?
         summary-name
         summary-variant
         summary-worker
         summary-count
         summary-real-mean
         summary-real-stddev
         summary-real-min
         summary-real-max
         summary-cpu-mean
         summary-gc-mean)

(struct stat (count sum sumsq min max) #:mutable)
(struct summary (name variant worker count real-mean real-stddev real-min real-max cpu-mean gc-mean) #:transparent)

(define (make-stat)
  (stat 0 0.0 0.0 +inf.0 -inf.0))

(define (stat-add! st value)
  (define x (exact->inexact value))
  (set-stat-count! st (add1 (stat-count st)))
  (set-stat-sum! st (+ (stat-sum st) x))
  (set-stat-sumsq! st (+ (stat-sumsq st) (* x x)))
  (set-stat-min! st (min (stat-min st) x))
  (set-stat-max! st (max (stat-max st) x)))

(define (stat-mean st)
  (define n (stat-count st))
  (if (zero? n) 0 (/ (stat-sum st) n)))

(define (stat-stddev st)
  (define n (stat-count st))
  (cond
    [(<= n 1) 0]
    [else
     (define mean (stat-mean st))
     (define variance (/ (max 0 (- (stat-sumsq st) (* mean mean n))) (sub1 n)))
     (sqrt variance)]))

(define (safe-min st)
  (if (zero? (stat-count st)) 0 (stat-min st)))

(define (safe-max st)
  (if (zero? (stat-count st)) 0 (stat-max st)))

(define (extract-field key pairs)
  (define entry (assoc key pairs))
  (cond
    [(not entry) #f]
    [else
     (define tail (cdr entry))
     (cond
       [(null? tail) #f]
       [(and (pair? tail) (not (pair? (car tail)))) (car tail)]  ; Single value like (name foo) -> foo
       [else tail])]))

(define (extract-metric key metrics)
  (cond
    [(not metrics) #f]
    [(null? metrics) #f]
    [else
     (define entry (assoc key metrics))
     (cond
       [(not entry) #f]
       [(null? (cdr entry)) #f]
       [else (cadr entry)])]))

(define (update-aggregates! aggregates datum)
  (when (and (list? datum) (pair? datum) (eq? (car datum) 'benchmark))
    (define fields (cdr datum))
    (define name (extract-field 'name fields))
    (define variant (extract-field 'variant fields))
    (define metrics (extract-field 'metrics fields))
    (define params (extract-field 'params fields))
    (when (and name variant metrics)
      (define real (extract-metric 'real-ms metrics))
      (define cpu (extract-metric 'cpu-ms metrics))
      (define gc (extract-metric 'gc-ms metrics))
      (define workers (and params (extract-metric 'workers params)))
      (define key (list name variant workers))
      (define agg (hash-ref aggregates key
                             (λ ()
                               (define fresh (vector (make-stat) (make-stat) (make-stat)))
                               (hash-set! aggregates key fresh)
                               fresh)))
      (when real (stat-add! (vector-ref agg 0) real))
      (when cpu (stat-add! (vector-ref agg 1) cpu))
      (when gc (stat-add! (vector-ref agg 2) gc)))))

(define (read-datum in)
  (with-handlers ([exn:fail:read? (λ (_) #f)])
    (read in)))

(define (process-port in aggregates)
  (let loop ()
    (define datum (read-datum in))
    (cond
      [(eof-object? datum) (void)]
      [datum (update-aggregates! aggregates datum) (loop)]
      [else (loop)])))

(define (load-summaries paths)
  (define aggregates (make-hash))
  (define files (if (null? paths) (list "-") paths))
  (for ([path (in-list files)])
    (cond
      [(string=? path "-") (process-port (current-input-port) aggregates)]
      [else (call-with-input-file path (λ (in) (process-port in aggregates)))]))
  (for/list ([(key stats) (in-hash aggregates)])
    (define name (first key))
    (define variant (second key))
    (define worker (third key))
    (define real-stat (vector-ref stats 0))
    (define cpu-stat (vector-ref stats 1))
    (define gc-stat (vector-ref stats 2))
    (summary name variant worker
             (stat-count real-stat)
             (stat-mean real-stat)
             (stat-stddev real-stat)
             (safe-min real-stat)
             (safe-max real-stat)
             (stat-mean cpu-stat)
             (stat-mean gc-stat))))
