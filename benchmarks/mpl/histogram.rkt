#lang racket

(require racket/fixnum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide histogram-sequential
         histogram-parallel)

;; ============================================================================
;; Sequential histogram
;; ============================================================================

(define (histogram-sequential data buckets)
  (define counts (make-vector buckets 0))
  (for ([val (in-vector data)])
    (define idx (fxmodulo val buckets))
    (vector-set! counts idx (fx+ 1 (vector-ref counts idx))))
  counts)

;; ============================================================================
;; Parallel histogram with local histograms + parallel merge
;; ============================================================================

(define (histogram-parallel data buckets workers)
  (define n (vector-length data))
  (define chunk-size (quotient (+ n workers -1) workers))

  ;; Phase 1: Parallel local histogram computation
  (define local-hists
    (let ([channels
           (for/list ([w (in-range workers)])
             (define start (* w chunk-size))
             (define end (min (+ start chunk-size) n))
             (if (>= start n)
                 #f
                 (let ([ch (make-channel)])
                   (thread
                    (lambda ()
                      (define local-counts (make-vector buckets 0))
                      (for ([i (in-range start end)])
                        (define val (vector-ref data i))
                        (define idx (fxmodulo val buckets))
                        (vector-set! local-counts idx
                                     (fx+ 1 (vector-ref local-counts idx))))
                      (channel-put ch local-counts)))
                   ch)))])
      (for/list ([ch channels] #:when ch) (channel-get ch))))

  ;; Phase 2: Parallel merge
  ;; Each worker merges a portion of the buckets
  (define result (make-vector buckets 0))
  (define bucket-chunk (quotient (+ buckets workers -1) workers))

  (let ([threads
         (for/list ([w (in-range workers)])
           (define bucket-start (* w bucket-chunk))
           (define bucket-end (min (+ bucket-start bucket-chunk) buckets))
           (if (>= bucket-start buckets)
               #f
               (thread
                (lambda ()
                  (for ([b (in-range bucket-start bucket-end)])
                    (define sum 0)
                    (for ([local-counts (in-list local-hists)])
                      (set! sum (fx+ sum (vector-ref local-counts b))))
                    (vector-set! result b sum))))))])
    (for ([t threads] #:when t) (thread-wait t)))

  result)

;; ============================================================================
;; Utilities
;; ============================================================================

(define (generate-random-data n range seed)
  (random-seed seed)
  (for/vector ([i (in-range n)])
    (random range)))

(define (verify-histograms h1 h2)
  (and (= (vector-length h1) (vector-length h2))
       (for/and ([i (in-range (vector-length h1))])
         (= (vector-ref h1 i) (vector-ref h2 i)))))

;; ============================================================================
;; Main
;; ============================================================================

(module+ main
  (define n 10000000)
  (define buckets 256)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "histogram.rkt"
    #:once-each
    [("--n") arg "Number of elements"
     (set! n (parse-positive-integer arg 'histogram))]
    [("--buckets" "-b") arg "Number of histogram buckets"
     (set! buckets (parse-positive-integer arg 'histogram))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'histogram))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'histogram))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'histogram))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (printf "Generating random data (n=~a, range=[0,~a))...\n" n buckets)
  (define data (generate-random-data n buckets seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'buckets buckets)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential histogram...\n")
    (set! seq-result
      (run-benchmark
       (lambda () (histogram-sequential data buckets))
       #:name 'histogram
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel histogram (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (lambda () (histogram-parallel data buckets workers))
     #:name 'histogram
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (lambda (iteration result)
               (when (and seq-result (not (verify-histograms seq-result result)))
                 (error 'histogram "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (verify-histograms seq-result par-result)
        (printf "Sequential and parallel results match\n")
        (printf "Results differ!\n")))

  (printf "\nSample counts (first 10 buckets):\n")
  (for ([i (in-range (min 10 buckets))])
    (printf "  bucket[~a] = ~a\n" i (vector-ref par-result i))))
