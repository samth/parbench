#lang racket

(require racket/async-channel
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide binary-trees)

(define (tree-check depth)
  (define (loop d)
    (if (zero? d)
        1
        (+ 1 (loop (sub1 d)) (loop (sub1 d)))))
  (loop depth))

(define (iterations-for-depth max-depth depth min-depth)
  (arithmetic-shift 1 (- (+ max-depth min-depth) depth)))

(define (sum-checks depth iterations workers)
  (define worker-count (max 1 workers))
  (define chunk-size (max 1 (ceiling (/ iterations worker-count))))
  (cond
    [(= worker-count 1)
     (for/sum ([i (in-range iterations)]) (tree-check depth))]
    [else
     ;; Create parallel thread pool
     (define pool (make-parallel-thread-pool worker-count))
     (define ranges (for/list ([start (in-range 0 iterations chunk-size)])
                      (cons start (min iterations (+ start chunk-size)))))
     (define ch (make-channel))
     ;; Spawn parallel threads
     (define threads
       (for/list ([rg (in-list ranges)])
         (define start (car rg))
         (define end (cdr rg))
         (thread
          #:pool pool
          (λ ()
            (define sum 0)
            (for ([i (in-range start end)])
              (set! sum (+ sum (tree-check depth))))
            (channel-put ch sum)))))
     ;; Collect results
     (define result
       (for/sum ([i (in-range (length ranges))])
         (channel-get ch)))
     ;; Wait for all threads
     (for ([t (in-list threads)])
       (thread-wait t))
     result]))

(define (binary-trees max-depth #:workers [workers 1] #:min-depth [min-depth 4])
  (define stretch-depth (add1 max-depth))
  (define stretch-check (tree-check stretch-depth))
  (define long-lived-depth max-depth)
  (define long-lived-check (tree-check long-lived-depth))
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
