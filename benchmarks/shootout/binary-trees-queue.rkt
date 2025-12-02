#lang racket/base

(require racket/match
         racket/require
         racket/async-channel
         racket/list
         (for-syntax racket/base)
         (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops)
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         racket/cmdline)

(provide binary-trees)

;; Tree structure: either a leaf with a value, or a node with value and two children
(struct *leaf (val))
(struct *node *leaf (left right))

(define-syntax leaf  (make-rename-transformer #'*leaf))
(define-syntax leaf? (make-rename-transformer #'*leaf?))
(define-syntax node  (make-rename-transformer #'*node))
(define-syntax node? (make-rename-transformer #'*node?))
(define-syntax-rule (leaf-val l)   (struct-ref l 0))
(define-syntax-rule (node-left n)  (struct-ref n 1))
(define-syntax-rule (node-right n) (struct-ref n 2))

;; Create a tree with given item value and depth
(define (make item d)
  (if (fx= d 0)
      (leaf item)
      (let ([item2 (fx* item 2)] [d2 (fx- d 1)])
        (node item (make (fx- item2 1) d2) (make item2 d2)))))

;; Check a tree (sum up all node values with specific traversal)
(define (check t)
  (let loop ([t t] [acc 0])
    (let ([acc (fx+ (leaf-val t) acc)])
      (if (node? t)
          (loop (node-left t)
                (fx- acc (loop (node-right t) 0)))
          acc))))

(define min-depth 4)

;; Sequential implementation
(define (binary-trees-sequential max-depth)
  (define stretch-depth (fx+ max-depth 1))
  (define stretch-check (check (make 0 stretch-depth)))
  (define long-lived-tree (make 0 max-depth))
  (define results
    (for/list ([d (in-range 4 (fx+ max-depth 1) 2)])
      (define iterations (fxlshift 1 (fx+ (fx- max-depth d) min-depth)))
      (define sum-check
        (for/fold ([c 0]) ([i (in-range iterations)])
          (fx+ c (fx+ (check (make i d))
                      (check (make (fx- 0 i) d))))))
      (list (fx* 2 iterations) d sum-check)))
  (define long-lived-check (check long-lived-tree))
  (list (list 'stretch stretch-depth stretch-check)
        (cons 'results results)
        (list 'long-lived max-depth long-lived-check)))

;; Parallel implementation using queue-based task distribution (SBCL-style)
(define (binary-trees-parallel max-depth workers)
  (define stretch-depth (fx+ max-depth 1))
  (define stretch-check (check (make 0 stretch-depth)))
  (define long-lived-tree (make 0 max-depth))

  ;; Create task queue with depth values
  (define task-queue (make-async-channel))
  (define output-queue (make-async-channel))

  ;; Enqueue all depth tasks
  (for ([d (in-range 4 (fx+ max-depth 1) 2)])
    (async-channel-put task-queue d))

  ;; Send termination signals (one per worker)
  (for ([_ (in-range workers)])
    (async-channel-put task-queue #f))

  ;; Worker function: dequeue tasks and process them
  (define (worker)
    (let loop ()
      (define depth (async-channel-get task-queue))
      (when depth
        (define iterations (fxlshift 1 (fx+ (fx- max-depth depth) min-depth)))
        (define sum-check
          (for/fold ([c 0]) ([i (in-range iterations)])
            (fx+ c (fx+ (check (make i depth))
                        (check (make (fx- 0 i) depth))))))
        (async-channel-put output-queue
                          (list depth (fx* 2 iterations) sum-check))
        (loop))))

  ;; Create thread pool and spawn worker threads
  (define pool (make-parallel-thread-pool workers))
  (define thds
    (for/list ([_ (in-range workers)])
      (thread #:pool pool #:keep 'results worker)))

  ;; Wait for all threads to complete
  (for-each thread-wait thds)
  (parallel-thread-pool-close pool)

  ;; Collect and sort results by depth
  (define result-count (quotient (+ (- max-depth 4) 2) 2))
  (define results
    (sort (for/list ([_ (in-range result-count)])
            (define r (async-channel-get output-queue))
            (match r
              [(list d iters check-sum)
               (list iters d check-sum)]))
          < #:key cadr))

  (define long-lived-check (check long-lived-tree))
  (list (list 'stretch stretch-depth stretch-check)
        (cons 'results results)
        (list 'long-lived max-depth long-lived-check)))

;; Main entry point
(define (binary-trees max-depth #:workers [workers 1] #:min-depth [_min-depth 4])
  (if (fx= workers 1)
      (binary-trees-sequential max-depth)
      (binary-trees-parallel max-depth workers)))

(module+ main
  (require racket/future)

  (define max-depth 16)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "binary-trees-queue.rkt"
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
     #:name 'binary-trees-queue
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (binary-trees max-depth #:workers workers))
   #:name 'binary-trees-queue
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (λ (_ value)
             (unless (equal? value sequential)
               (error 'binary-trees "parallel mismatch"))))

  (close-log-writer writer))
