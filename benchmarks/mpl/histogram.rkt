#lang racket

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide histogram-sequential
         histogram-parallel)

;; Sequential histogram: simple array accumulation
(define (histogram-sequential data buckets)
  (define counts (make-vector buckets 0))
  (for ([val (in-vector data)])
    (define idx (modulo val buckets))
    (vector-set! counts idx (fx+ 1 (vector-ref counts idx))))
  counts)

;; Parallel histogram: partition data, compute local histograms, then merge
(define (histogram-parallel data buckets workers)
  (define n (vector-length data))
  (define chunk-size (quotient (+ n workers -1) workers))

  ;; Compute local histograms in parallel
  (define local-hists
    (for/list ([w (in-range workers)])
      (define start (* w chunk-size))
      (define end (min (+ start chunk-size) n))
      (future
       (λ ()
         (define local-counts (make-vector buckets 0))
         (for ([i (in-range start end)])
           (define val (vector-ref data i))
           (define idx (modulo val buckets))
           (vector-set! local-counts idx (fx+ 1 (vector-ref local-counts idx))))
         local-counts))))

  ;; Wait for all futures and merge results
  (define result (make-vector buckets 0))
  (for ([fut (in-list local-hists)])
    (define local-counts (touch fut))
    (for ([bucket (in-range buckets)])
      (vector-set! result bucket
                   (fx+ (vector-ref result bucket)
                        (vector-ref local-counts bucket)))))
  result)

;; Generate random test data
(define (generate-random-data n range seed)
  (random-seed seed)
  (for/vector ([i (in-range n)])
    (random range)))

;; Verify that two histograms are equal
(define (verify-histograms h1 h2)
  (and (= (vector-length h1) (vector-length h2))
       (for/and ([i (in-range (vector-length h1))])
         (= (vector-ref h1 i) (vector-ref h2 i)))))

(module+ main
  (define n 10000000)
  (define buckets 256)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define strategy 'threads)

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
    [("--strategy") arg "Parallel strategy: threads or futures"
     (set! strategy (string->symbol arg))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]))

  (set-parallel-strategy! strategy)

  (printf "Generating random data (n=~a, range=[0,~a))...\n" n buckets)
  (define data (generate-random-data n buckets seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'buckets buckets)
                       (list 'workers workers)
                       (list 'seed seed)
                       (list 'strategy strategy)))

  (printf "Running sequential histogram...\n")
  (define seq-result
    (run-benchmark
     (λ () (histogram-sequential data buckets))
     #:name 'histogram
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel histogram (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (histogram-parallel data buckets workers))
     #:name 'histogram
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (verify-histograms seq-result result)
                 (error 'histogram "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (verify-histograms seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "\nSample counts (first 10 buckets):\n")
  (for ([i (in-range (min 10 buckets))])
    (printf "  bucket[~a] = ~a\n" i (vector-ref seq-result i))))
