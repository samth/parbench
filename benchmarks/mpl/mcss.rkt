#lang racket

;; MCSS - Maximum Contiguous Subsequence Sum
;; Optimized to match MPL's algorithm with reduced allocations

(require racket/flonum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt")

(provide mcss-sequential
         mcss-parallel
         generate-random-data)

;; Generate random data for MCSS
(define (generate-random-data n seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (define data (make-flvector n))
    (for ([i (in-range n)])
      (flvector-set! data i (fl- (fl* 2.0 (random)) 1.0)))
    data))

;; 4-tuple reduction for MCSS
;; Tuple: (prefix, suffix, best, total)
;; - prefix: max sum starting from left edge
;; - suffix: max sum ending at right edge
;; - best: max sum of any contiguous subarray
;; - total: sum of all elements

;; Combine two tuples - associative operation for parallel reduction
;; (l1, r1, b1, t1) combine (l2, r2, b2, t2) =
;;   (max(l1, t1+l2), max(r2, r1+t2), max(b1, b2, r1+l2), t1+t2)
(define-syntax-rule (combine-tuples l1 r1 b1 t1 l2 r2 b2 t2)
  (values (flmax l1 (fl+ t1 l2))
          (flmax r2 (fl+ r1 t2))
          (flmax (flmax b1 b2) (fl+ r1 l2))
          (fl+ t1 t2)))

;; Sequential reduction over a range - optimized two-pass algorithm
;; Pass 1: left-to-right for total, prefix, and best
;; Pass 2: right-to-left for suffix
(define (reduce-range data lo hi)
  (cond
    [(= (- hi lo) 1)
     ;; Single element case
     (define v (flvector-ref data lo))
     (define vp (flmax v 0.0))
     (values vp vp vp v)]
    [else
     ;; Pass 1: left-to-right for total, prefix, and best
     (define-values (total prefix best)
       (let loop ([i lo]
                  [running-sum 0.0]
                  [prefix-max -inf.0]
                  [kadane-ending 0.0]
                  [kadane-best -inf.0])
         (if (>= i hi)
             (values running-sum (flmax prefix-max 0.0) (flmax kadane-best 0.0))
             (let* ([v (flvector-ref data i)]
                    [new-sum (fl+ running-sum v)]
                    [new-prefix (flmax prefix-max new-sum)]
                    [new-ending (flmax v (fl+ kadane-ending v))]
                    [new-best (flmax kadane-best new-ending)])
               (loop (add1 i) new-sum new-prefix new-ending new-best)))))

     ;; Pass 2: right-to-left for suffix
     (define suffix
       (let loop ([i (sub1 hi)]
                  [running-sum 0.0]
                  [suffix-max -inf.0])
         (if (< i lo)
             (flmax suffix-max 0.0)
             (let* ([v (flvector-ref data i)]
                    [new-sum (fl+ running-sum v)]
                    [new-suffix (flmax suffix-max new-sum)])
               (loop (sub1 i) new-sum new-suffix)))))

     (values prefix suffix best total)]))

;; Sequential MCSS using same 4-tuple reduction as parallel
;; Uses reduce-range over the entire array for fair comparison
(define (mcss-sequential data)
  (define n (flvector-length data))
  (when (= n 0) (error 'mcss "empty data"))
  (define-values (prefix suffix best total) (reduce-range data 0 n))
  best)

;; Parallel MCSS using flat chunking (like histogram)
;; Spawns exactly `workers` threads instead of recursive divide-and-conquer
(define (mcss-parallel data workers)
  (define n (flvector-length data))
  (when (= n 0) (error 'mcss "empty data"))
  (mcss-parallel-impl data workers n))

;; Parallel implementation
(define (mcss-parallel-impl data workers n)
  (define pool (make-parallel-thread-pool workers))
  (define chunk-size (quotient (+ n workers -1) workers))

  ;; Phase 1: Each worker reduces its chunk (exactly `workers` threads)
  (define channels
    (for/list ([w (in-range workers)])
      (define start (* w chunk-size))
      (define end (min (+ start chunk-size) n))
      (if (>= start n)
          #f
          (let ([ch (make-channel)])
            (thread #:pool pool
              (λ ()
                (define-values (l r b t) (reduce-range data start end))
                (channel-put ch (vector l r b t))))
            ch))))

  (define results
    (for/list ([ch channels] #:when ch)
      (channel-get ch)))

  (parallel-thread-pool-close pool)

  ;; Phase 2: Combine chunk results sequentially (workers-1 combines)
  (let loop ([results results])
    (cond
      [(= (length results) 1)
       (define r (first results))
       (vector-ref r 2)]  ;; Return 'best' from tuple
      [else
       (define r1 (first results))
       (define r2 (second results))
       (define-values (nl nr nb nt)
         (combine-tuples (vector-ref r1 0) (vector-ref r1 1) (vector-ref r1 2) (vector-ref r1 3)
                         (vector-ref r2 0) (vector-ref r2 1) (vector-ref r2 2) (vector-ref r2 3)))
       (loop (cons (vector nl nr nb nt) (cddr results)))])))

(module+ main
  (define n 1000000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")
  (define skip-sequential #f)

  (command-line
   #:program "mcss"
   #:once-each
   [("--n") arg "Problem size (default: 1000000)"
    (set! n (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)]
   [("--skip-sequential") "Skip sequential variant"
    (set! skip-sequential #t)])

  ;; Generate input data
  (define data (generate-random-data n seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'seed seed)
                       (list 'workers workers)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential mcss(n=~a)...\n" n)
    (set! seq-result
      (run-benchmark
       (λ () (mcss-sequential data))
       #:name 'mcss
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel mcss(n=~a) (workers=~a)...\n" n workers)
  (define par-result
    (run-benchmark
     (λ () (mcss-parallel data workers))
     #:name 'mcss
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (< (abs (- seq-result result)) 0.001)))
                 (error 'mcss "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (< (abs (- seq-result par-result)) 0.001)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "mcss = ~a\n" par-result))
