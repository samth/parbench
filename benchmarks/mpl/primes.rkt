#lang racket

;; Port of primes-segmented from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/primes-segmented
;; Adapted for Racket parallel benchmarking
;;
;; Segmented Sieve of Eratosthenes:
;; Finds all prime numbers up to n using a segmented approach for better memory locality.
;; Divides the range into blocks and sieves each block in parallel.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt")

(provide primes-sequential
         primes-parallel)

;; Simple sieve for small primes (up to sqrt(n))
(define (simple-sieve limit)
  (define flags (make-vector (add1 limit) #t))
  (vector-set! flags 0 #f)
  (vector-set! flags 1 #f)

  (for ([i (in-range 2 (add1 (integer-sqrt limit)))])
    (when (vector-ref flags i)
      (for ([j (in-range (* i i) (add1 limit) i)])
        (vector-set! flags j #f))))

  ;; Collect primes
  (for/list ([i (in-range 2 (add1 limit))]
             #:when (vector-ref flags i))
    i))

;; Sequential segmented sieve
(define (primes-sequential n)
  (when (<= n 1) (error 'primes "n must be > 1"))

  (define sqrt-n (integer-sqrt n))
  (define base-primes (simple-sieve sqrt-n))

  ;; Count primes in a segment [start, end)
  (define (sieve-segment start end)
    (define size (- end start))
    (define flags (make-vector size #t))

    ;; Mark multiples of each base prime
    (for ([p (in-list base-primes)])
      ;; Find first multiple of p >= start
      (define first-multiple
        (let ([r (modulo start p)])
          (if (= r 0)
              start
              (+ start (- p r)))))
      ;; Skip if first multiple is p itself (it's prime)
      (define begin-mark
        (if (< first-multiple (* p 2))
            (* p 2)
            first-multiple))
      ;; Mark multiples
      (for ([j (in-range begin-mark end p)])
        (vector-set! flags (- j start) #f)))

    ;; Count primes in this segment
    (for/sum ([i (in-range size)]
              #:when (vector-ref flags i))
      1))

  ;; Process segments sequentially
  (define block-size (* sqrt-n 8))  ; Tune for performance
  (+ (length base-primes)  ; Primes up to sqrt-n
     (for/sum ([start (in-range (add1 sqrt-n) (add1 n) block-size)])
       (define end (min (add1 n) (+ start block-size)))
       (sieve-segment start end))))

;; Parallel segmented sieve
(define (primes-parallel n workers)
  (when (<= n 1) (error 'primes "n must be > 1"))

  (define sqrt-n (integer-sqrt n))
  (define base-primes (simple-sieve sqrt-n))

  ;; Count primes in a segment [start, end)
  (define (sieve-segment start end)
    (define size (- end start))
    (define flags (make-vector size #t))

    ;; Mark multiples of each base prime
    (for ([p (in-list base-primes)])
      ;; Find first multiple of p >= start
      (define first-multiple
        (let ([r (modulo start p)])
          (if (= r 0)
              start
              (+ start (- p r)))))
      ;; Skip if first multiple is p itself
      (define begin-mark
        (if (< first-multiple (* p 2))
            (* p 2)
            first-multiple))
      ;; Mark multiples
      (for ([j (in-range begin-mark end p)])
        (vector-set! flags (- j start) #f)))

    ;; Count primes in this segment
    (for/sum ([i (in-range size)]
              #:when (vector-ref flags i))
      1))

  ;; Divide range into segments
  (define block-size (* sqrt-n 8))
  (define segments
    (for/list ([start (in-range (add1 sqrt-n) (add1 n) block-size)])
      (cons start (min (add1 n) (+ start block-size)))))

  (+ (length base-primes)
     (if (<= workers 1)
         (for/sum ([seg (in-list segments)])
           (sieve-segment (car seg) (cdr seg)))
         ;; Parallel: chunk segments across workers
         (let* ([pool (make-parallel-thread-pool workers)]
                [seg-vec (list->vector segments)]
                [num-segs (vector-length seg-vec)]
                [chunk-size (quotient (+ num-segs workers -1) workers)]
                [channels
                 (for/list ([w (in-range workers)])
                   (define start-idx (* w chunk-size))
                   (define end-idx (min (+ start-idx chunk-size) num-segs))
                   (if (>= start-idx num-segs)
                       #f
                       (let ([ch (make-channel)])
                         (thread #:pool pool
                          (λ ()
                            (channel-put ch
                              (for/sum ([i (in-range start-idx end-idx)])
                                (define seg (vector-ref seg-vec i))
                                (sieve-segment (car seg) (cdr seg))))))
                         ch)))]
                [result (apply + (for/list ([ch channels] #:when ch) (channel-get ch)))])
           (parallel-thread-pool-close pool)
           result))))

(module+ main
  (define n 10000000)
  (define workers 1)
  (define repeat 1)
  (define log-path "")
  (define skip-sequential #f)

  (command-line
   #:program "primes"
   #:once-each
   [("--n") arg "Upper limit for primes (default: 10000000)"
    (set! n (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)]
   [("--skip-sequential") "Skip sequential variant"
    (set! skip-sequential #t)])

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'workers workers)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential primes(n=~a)...\n" n)
    (set! seq-result
      (run-benchmark
       (λ () (primes-sequential n))
       #:name 'primes
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel primes(n=~a) (workers=~a)...\n" n workers)
  (define par-result
    (run-benchmark
     (λ () (primes-parallel n workers))
     #:name 'primes
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (= seq-result result)))
                 (error 'primes "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (= seq-result par-result)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "Found ~a primes up to ~a\n" par-result n))
