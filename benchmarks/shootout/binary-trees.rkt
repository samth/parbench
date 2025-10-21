#lang racket

(require "../common/parallel.rkt"
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide binary-trees)

(struct tree (left right) #:transparent)

(define (make-tree depth)
  (if (zero? depth)
      (tree #f #f)
      (tree (make-tree (sub1 depth))
            (make-tree (sub1 depth)))))

(define (check-tree node)
  (if (tree-left node)
      (+ 1
         (check-tree (tree-left node))
         (check-tree (tree-right node)))
      1))

(define (iterations-for-depth max-depth depth min-depth)
  (arithmetic-shift 1 (- (+ max-depth min-depth) depth)))

(define (sum-checks depth iterations workers)
  (define worker-count (max 1 workers))
  (cond
    [(= worker-count 1)
     (for/sum ([_ (in-range iterations)])
       (check-tree (make-tree depth)))]
    [else
     (define chunk-size
       (max 1 (ceiling (/ iterations worker-count))))
     (define ranges
       (for/list ([start (in-range 0 iterations chunk-size)])
         (cons start (min iterations (+ start chunk-size)))))
     (call-with-thread-pool worker-count
       (λ (pool actual-workers)
         (define partial-sums
           (thread-pool-wait/collect
            (for/list ([rg (in-list ranges)])
              (define start (car rg))
              (define end (cdr rg))
              (thread-pool-submit
               pool
               (λ ()
                 (for/sum ([_ (in-range start end)])
                   (check-tree (make-tree depth))))))))
         (for/sum ([value (in-list partial-sums)])
           (if (and (list? value) (pair? value))
               (car value)
               value)))
       #:max (length ranges))]))

(define (binary-trees max-depth #:workers [workers 1] #:min-depth [min-depth 4])
  (define stretch-depth (add1 max-depth))
  (define stretch-check (check-tree (make-tree stretch-depth)))
  (define long-lived-depth max-depth)
  (define long-lived-tree (make-tree long-lived-depth))
  (define long-lived-check (check-tree long-lived-tree))
  (define results
    (for/list ([depth (in-range min-depth (add1 max-depth) 2)])
      (define iterations (iterations-for-depth max-depth depth min-depth))
      (define check (sum-checks depth iterations workers))
      (list depth iterations check)))
  (list (list 'stretch stretch-depth stretch-check)
        (cons 'results results)
        (list 'long-lived long-lived-depth long-lived-check)))

(module+ main
  (define max-depth 16)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "binary-trees.rkt"
    #:once-each
    [("--max-depth") arg "Maximum tree depth"
     (set! max-depth (parse-positive-integer arg 'binary-trees))]
    [("--workers") arg "Parallel thread count"
     (set! workers (parse-positive-integer arg 'binary-trees))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'binary-trees))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'max-depth max-depth)
                       (list 'workers workers)))

  (define sequential
    (run-benchmark
     (λ () (binary-trees max-depth #:workers 1))
     #:name 'binary-trees
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (binary-trees max-depth #:workers workers))
   #:name 'binary-trees
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (λ (_ value)
             (unless (equal? value sequential)
               (error 'binary-trees "parallel mismatch"))))

  (close-log-writer writer))
