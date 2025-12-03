#lang racket

(require racket/list
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide fannkuch-redux)

(define (count-flips pi rho)
  (vector-copy! rho 0 pi)
  (let loop ([i 0])
    (if (unsafe-fx= (unsafe-vector-ref rho 0) 0)
        i
        (begin
          (vector-reverse-slice! rho 0 (unsafe-fx+ 1 (unsafe-vector-ref rho 0)))
          (loop (unsafe-fx+ 1 i))))))

(define (vector-reverse-slice! v i j)
  (let loop ([i i]
             [j (unsafe-fx- j 1)])
    (when (unsafe-fx> j i)
      (vector-swap! v i j)
      (loop (unsafe-fx+ 1 i) (unsafe-fx- j 1)))))

(define-syntax-rule (vector-swap! v i j)
  (let ((t (unsafe-vector-ref v i)))
    (unsafe-vector-set! v i (unsafe-vector-ref v j))
    (unsafe-vector-set! v j t)))

(define (factorial n)
  (for/fold ([acc 1]) ([i (in-range 2 (add1 n))])
    (* acc i)))

(define (reverse-suffix! v start end)
  (let loop ([i start] [j end])
    (when (unsafe-fx< i j)
      (vector-swap! v i j)
      (loop (unsafe-fx+ i 1) (unsafe-fx- j 1)))))

(define (permutation-from-index n idx)
  (define digits (make-vector n 0))
  (define temp idx)
  (for ([i (in-range 1 (add1 n))])
    (vector-set! digits (- n i) (modulo temp i))
    (set! temp (quotient temp i)))
  (define available (for/list ([i (in-range n)]) i))
  (define perm (make-vector n 0))
  (for ([i (in-range n)])
    (define d (vector-ref digits i))
    (define val (list-ref available d))
    (vector-set! perm i val)
    (set! available (append (take available d) (drop available (add1 d)))))
  perm)

(define (next-permutation! perm)
  (define n (vector-length perm))
  (let loop ([i (unsafe-fx- n 2)])
    (cond
      [(unsafe-fx< i 0) #f]
      [(unsafe-fx< (unsafe-vector-ref perm i)
                   (unsafe-vector-ref perm (unsafe-fx+ i 1)))
       (let search ([j (unsafe-fx- n 1)])
         (if (unsafe-fx> (unsafe-vector-ref perm j)
                         (unsafe-vector-ref perm i))
             (begin
               (vector-swap! perm i j)
               (reverse-suffix! perm (unsafe-fx+ i 1) (unsafe-fx- n 1))
               #t)
             (search (unsafe-fx- j 1))))]
      [else (loop (unsafe-fx- i 1))])))

(define (fannkuch-range n start count)
  (define total (factorial n))
  (define end (min total (unsafe-fx+ start count)))
  (define perm (permutation-from-index n start))
  (define tmp (make-vector n))
  (define max-flips 0)
  (define checksum 0)
  (let loop ([idx start])
    (when (unsafe-fx< idx end)
      (define flips (count-flips perm tmp))
      (when (> flips max-flips) (set! max-flips flips))
      (define sign (if (even? idx) 1 -1))
      (set! checksum (+ checksum (* sign flips)))
      (define next-idx (unsafe-fx+ idx 1))
      (when (and (unsafe-fx< next-idx end)
                 (next-permutation! perm))
        (loop next-idx))))
  (values max-flips checksum))

(define (fannkuch-sequential n)
  (fannkuch-range n 0 (factorial n)))

(define (fannkuch-parallel n workers)
  (define total (factorial n))
  (define worker-count (max 1 (min workers total)))
  (define chunk-size (max 1 (quotient (+ total worker-count -1) worker-count)))
  (define pool (make-parallel-thread-pool worker-count))
  (define thds
    (for/list ([start (in-range 0 total chunk-size)])
      (define remaining (- total start))
      (define count (min remaining chunk-size))
      (thread #:pool pool #:keep 'results
        (λ () (call-with-values (λ () (fannkuch-range n start count)) list)))))
  (define results (map thread-wait thds))
  (parallel-thread-pool-close pool)
  (define max-flips 0)
  (define checksum 0)
  (for ([vals (in-list results)])
    (define flips (first vals))
    (define partial-sum (second vals))
    (set! max-flips (max max-flips flips))
    (set! checksum (+ checksum partial-sum)))
  (values max-flips checksum))

(define (fannkuch-redux n #:workers [workers 1])
  (if (= workers 1)
      (fannkuch-sequential n)
      (fannkuch-parallel n workers)))

(module+ main
  (define n 10)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "fannkuch-redux.rkt"
    #:once-each
    [("--n") arg "Permutation size"
     (set! n (parse-positive-integer arg 'fannkuch-redux))]
    [("--workers") arg "Parallel thread count"
     (set! workers (parse-positive-integer arg 'fannkuch-redux))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'fannkuch-redux))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n) (list 'workers workers)))

  (define sequential
    (run-benchmark
     (λ () (call-with-values (λ () (fannkuch-redux n #:workers 1)) list))
     #:name 'fannkuch-redux
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (call-with-values (λ () (fannkuch-redux n #:workers workers)) list))
   #:name 'fannkuch-redux
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (λ (_ value)
             (unless (equal? value sequential)
               (error 'fannkuch-redux "parallel mismatch: ~a vs ~a" value sequential))))

  (close-log-writer writer))
