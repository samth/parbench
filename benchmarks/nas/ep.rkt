#lang racket

(require racket/math
         racket/vector
         "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt"
         "../common/run.rkt"
         "common/classes.rkt")

(provide ep
         ep-stats≈
         (struct-out ep-stats))

(struct ep-stats (sx sy counts) #:transparent)

(define stats-tolerance 1e-9)

(define (ep-stats≈ a b)
  (and (<= (abs (- (ep-stats-sx a) (ep-stats-sx b))) stats-tolerance)
       (<= (abs (- (ep-stats-sy a) (ep-stats-sy b))) stats-tolerance)
       (equal? (ep-stats-counts a) (ep-stats-counts b))))

(define modulus (expt 2 46))
(define multiplier 1220703125)
(define base-seed 271828183)
(define two-pi (* 2.0 pi))
(define bin-count 10)

(define (mod-pow base exp mod)
  (let loop ([result 1]
             [base (modulo base mod)]
             [exp exp])
    (cond
      [(zero? exp) result]
      [(even? exp)
       (loop result (modulo (* base base) mod) (quotient exp 2))]
      [else
       (loop (modulo (* result base) mod)
             (modulo (* base base) mod)
             (quotient (sub1 exp) 2))])))

(define (lcg-next seed)
  (define next (modulo (* seed multiplier) modulus))
  (values next (/ (exact->inexact next) modulus)))

(define (lcg-advance seed steps)
  (if (zero? steps)
      seed
      (modulo (* seed (mod-pow multiplier steps modulus)) modulus)))

(define (ep-bin-index radius)
  (define idx (inexact->exact (floor radius)))
  (cond
    [(< idx 0) 0]
    [(>= idx bin-count) (sub1 bin-count)]
    [else idx]))

(define (ep-chunk start count)
  (define local-seed (lcg-advance base-seed (* 2 start)))
  (define sx 0.0)
  (define sy 0.0)
  (define counts (make-vector bin-count 0))
  (for ([i count])
    (define-values (seed1 u1) (lcg-next local-seed))
    (set! local-seed seed1)
    (define-values (seed2 u2) (lcg-next local-seed))
    (set! local-seed seed2)
    (define u1* (if (zero? u1) (/ 1.0 modulus) u1))
    (define radius (sqrt (* -2.0 (log u1*))))
    (define theta (* two-pi u2))
    (define x (* radius (cos theta)))
    (define y (* radius (sin theta)))
    (set! sx (+ sx x))
    (set! sy (+ sy y))
    (define idx (ep-bin-index radius))
    (vector-set! counts idx (add1 (vector-ref counts idx))))
  (ep-stats sx sy counts))

(define (combine-stats a b)
  (define combined-counts (vector-copy (ep-stats-counts a)))
  (for ([i (in-range bin-count)])
    (vector-set! combined-counts i
                 (+ (vector-ref combined-counts i)
                    (vector-ref (ep-stats-counts b) i))))
  (ep-stats (+ (ep-stats-sx a) (ep-stats-sx b))
            (+ (ep-stats-sy a) (ep-stats-sy b))
            combined-counts))

(define (ep-sequential pairs)
  (ep-chunk 0 pairs))

(define (ep-parallel pairs workers)
  (call-with-thread-pool workers
    (λ (pool actual-workers)
      (define chunk-size (max 1 (quotient (+ pairs actual-workers -1) actual-workers)))
      (define tasks
        (for/list ([w (in-range actual-workers)])
          (define start (* w chunk-size))
          (define end (min pairs (+ start chunk-size)))
          (and (< start end)
               (thread-pool-submit
                pool
                (λ () (ep-chunk start (- end start)))))))
      (define results
        (for/list ([task (in-list tasks)] #:when task)
          (thread-pool-wait task)))
      (cond
        [(null? results) (ep-sequential pairs)]
        [else
         (for/fold ([acc (car results)])
                   ([res (in-list (cdr results))])
           (combine-stats acc res))]))
    #:max (if (zero? pairs) 1 pairs)))

(define (ensure-class sym)
  (define class (string->symbol (string-upcase (symbol->string sym))))
  (unless (hash-has-key? ep-class->pairs class)
    (error 'ep "unknown NAS class ~a" sym))
  class)

(define (resolve-pairs class override)
  (cond
    [override override]
    [else (hash-ref ep-class->pairs class
                    (λ () (error 'ep "no EP parameters for class ~a" class)))]))

(define (ep #:class [class default-nas-class]
            #:workers [workers 1]
            #:pairs [pair-override #f])
  (define class* (ensure-class class))
  (define total-pairs (resolve-pairs class* pair-override))
  (cond
    [(<= workers 1) (ep-sequential total-pairs)]
    [else (ep-parallel total-pairs workers)]))

(module+ main
  (define class default-nas-class)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define pair-override #f)

  (void
   (command-line
    #:program "ep.rkt"
    #:once-each
    [("--class") arg "NAS problem class (S, W, A, B, C)"
     (set! class (string->symbol (string-upcase arg)))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'nas-ep))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'nas-ep))]
    [("--pairs") arg "Override pair count (testing)"
     (set! pair-override (string->number arg))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define actual-class (ensure-class class))
  (define actual-pairs (resolve-pairs actual-class pair-override))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'class actual-class)
                       (list 'pairs actual-pairs)
                       (list 'workers workers)))

  (define sequential-result
    (run-benchmark
     (λ () (ep #:class actual-class #:pairs actual-pairs #:workers 1))
     #:name 'nas-ep
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (define _parallel-result
    (run-benchmark
     (λ () (ep #:class actual-class #:pairs actual-pairs #:workers workers))
     #:name 'nas-ep
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (_ value)
               (unless (ep-stats≈ value sequential-result)
                 (error 'nas-ep "parallel mismatch")))))

  (close-log-writer writer))
