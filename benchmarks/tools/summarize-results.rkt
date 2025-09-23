#lang racket

(require racket/cmdline
         racket/list
         racket/math
         racket/format)

(struct stat (count sum sumsq min max) #:mutable)

(define (make-stat)
  (stat 0 0.0 0.0 +inf.0 -inf.0))

(define (stat-add! st x)
  (define value (exact->inexact x))
  (set-stat-count! st (add1 (stat-count st)))
  (set-stat-sum! st (+ (stat-sum st) value))
  (set-stat-sumsq! st (+ (stat-sumsq st) (* value value)))
  (set-stat-min! st (min value (stat-min st)))
  (set-stat-max! st (max value (stat-max st))))

(define (stat-mean st)
  (if (zero? (stat-count st))
      0
      (/ (stat-sum st) (stat-count st))))

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

(struct aggregate (name variant cpu real count) #:mutable)

(define (make-aggregate)
  (aggregate #f #f (make-stat) (make-stat) 0))

(define aggregates (make-hash))

(define (extract-field key fields)
  (define entry (assoc key fields))
  (cond
    [(pair? entry)
     (define tail (cdr entry))
     (cond
       [(null? tail) #f]
       [(null? (cdr tail)) (car tail)]
       [else tail])]
    [else #f]))

(define (extract-metric key metrics)
  (define entry (assoc key metrics))
  (cond
    [(pair? entry)
     (define tail (cdr entry))
     (if (null? tail) #f (car tail))]
    [else #f]))

(define (process-event datum)
  (when (and (list? datum) (pair? datum) (eq? (car datum) 'benchmark))
    (define fields (cdr datum))
    (define name (extract-field 'name fields))
    (define variant (extract-field 'variant fields))
    (define metrics (extract-field 'metrics fields))
    (when (and name variant metrics)
      (define cpu (extract-metric 'cpu-ms metrics))
      (define real (extract-metric 'real-ms metrics))
      (define key (cons name variant))
      (define agg (hash-ref aggregates key
                             (λ ()
                               (define fresh (make-aggregate))
                               (set-aggregate-name! fresh name)
                               (set-aggregate-variant! fresh variant)
                               (hash-set! aggregates key fresh)
                               fresh)))
      (when cpu (stat-add! (aggregate-cpu agg) cpu))
      (when real (stat-add! (aggregate-real agg) real))
      (set-aggregate-count! agg (add1 (aggregate-count agg))))))

(define (process-port in)
  (let loop ()
    (define datum (with-handlers ([exn:fail:read? (λ (_) #f)]) (read in)))
    (cond
      [(eof-object? datum) (void)]
      [datum (process-event datum) (loop)]
      [else (void)])))

(define (display-summary)
  (define rows
    (for/list ([(key agg) (in-hash aggregates)])
      (list (aggregate-name agg)
            (aggregate-variant agg)
            (aggregate-count agg)
            (stat-mean (aggregate-real agg))
            (stat-stddev (aggregate-real agg))
            (safe-min (aggregate-real agg))
            (safe-max (aggregate-real agg))
            (stat-mean (aggregate-cpu agg)))))
  (define sorted
    (sort rows
          (λ (a b)
            (< (list-ref a 3) (list-ref b 3)))))
  (printf "~a\n" "name variant count real-mean(ms) real-stddev real-min real-max cpu-mean(ms)")
  (for ([row sorted])
    (apply printf "~a ~a ~a ~a ~a ~a ~a ~a\n"
           (map (λ (x)
                  (cond
                    [(real? x) (real->decimal-string x 3)]
                    [else x]))
                row))))

(module+ main
  (define files '())
  (command-line
   #:program "summarize-results.rkt"
   #:once-each
   [("--file") path "Benchmark log file (S-expression format)."
    (set! files (cons path files))]
   #:args positional
   (set! files (append files positional)))
  (when (null? files)
    (set! files (list "-")))
  (for ([path (in-list files)])
    (cond
      [(string=? path "-") (process-port (current-input-port))]
      [else
       (call-with-input-file path
         process-port)]))
  (display-summary))
