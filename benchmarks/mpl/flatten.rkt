#lang racket

;; Port of flatten from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/flatten
;; Adapted for Racket parallel benchmarking
;;
;; Flatten nested sequences - tests parallel prefix sum (scan) and sequence operations

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide flatten-sequential
         flatten-parallel
         generate-nested-vector)

;; Sequential flatten - concatenate all nested vectors
(define (flatten-sequential nested)
  (define total-length
    (for/sum ([v (in-vector nested)])
      (vector-length v)))
  (define result (make-vector total-length))
  (define pos 0)
  (for ([v (in-vector nested)])
    (for ([x (in-vector v)])
      (vector-set! result pos x)
      (set! pos (+ pos 1))))
  result)

;; Parallel flatten using prefix sum for output positions
(define (flatten-parallel nested workers)
  (define n (vector-length nested))
  (call-with-thread-pool workers
    (λ (pool actual-workers)
      ;; Phase 1: Compute lengths in parallel
      (define lengths
        (let ([chunk-size (quotient (+ n actual-workers -1) actual-workers)])
          (apply vector-append
                 (thread-pool-wait/collect
                  (for/list ([w (in-range actual-workers)])
                    (define start (* w chunk-size))
                    (define end (min (+ start chunk-size) n))
                    (thread-pool-submit
                     pool
                     (λ ()
                       (for/vector ([i (in-range start end)])
                         (vector-length (vector-ref nested i))))))))))

      ;; Phase 2: Sequential prefix sum to find output positions
      (define positions (make-vector n 0))
      (define total 0)
      (for ([i (in-range n)])
        (vector-set! positions i total)
        (set! total (+ total (vector-ref lengths i))))

      ;; Phase 3: Copy elements in parallel
      (define result (make-vector total))
      (define chunk-size (quotient (+ n actual-workers -1) actual-workers))
      (thread-pool-wait/collect
       (for/list ([w (in-range actual-workers)])
         (define start (* w chunk-size))
         (define end (min (+ start chunk-size) n))
         (thread-pool-submit
          pool
          (λ ()
            (for ([i (in-range start end)])
              (define v (vector-ref nested i))
              (define pos (vector-ref positions i))
              (for ([j (in-range (vector-length v))])
                (vector-set! result (+ pos j) (vector-ref v j))))))))

      result)
    #:max n))

;; Generate nested vector with varying sub-vector sizes
(define (generate-nested-vector n avg-size seed)
  (random-seed seed)
  (for/vector ([i (in-range n)])
    ;; Vary size from 0 to 2*avg-size
    (define size (random (* 2 avg-size)))
    (for/vector ([j (in-range size)])
      (+ (* i 1000) j))))

;; Verify two flat vectors are equal
(define (vectors-equal? v1 v2)
  (and (= (vector-length v1) (vector-length v2))
       (for/and ([i (in-range (vector-length v1))])
         (= (vector-ref v1 i) (vector-ref v2 i)))))

(module+ main
  (define n 10000)
  (define avg-size 100)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)

  (void
   (command-line
    #:program "flatten.rkt"
    #:once-each
    [("--n") arg "Number of nested vectors"
     (set! n (parse-positive-integer arg 'flatten))]
    [("--avg-size") arg "Average size of nested vectors"
     (set! avg-size (parse-positive-integer arg 'flatten))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'flatten))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'flatten))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'flatten))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]))

  (printf "Generating nested vectors (n=~a, avg-size=~a)...\n" n avg-size)
  (define nested (generate-nested-vector n avg-size seed))

  (define total-elements
    (for/sum ([v (in-vector nested)])
      (vector-length v)))
  (printf "Total elements to flatten: ~a\n" total-elements)

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'avg-size avg-size)
                       (list 'total-elements total-elements)
                       (list 'workers workers)
                       (list 'seed seed)))

  (printf "Running sequential flatten...\n")
  (define seq-result
    (run-benchmark
     (λ () (flatten-sequential nested))
     #:name 'flatten
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel flatten (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (flatten-parallel nested workers))
     #:name 'flatten
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (vectors-equal? seq-result result)
                 (error 'flatten "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (vectors-equal? seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "Flattened length: ~a elements\n" (vector-length seq-result))

  (when (<= total-elements 50)
    (printf "\nNested structure:\n")
    (for ([i (in-range (min 5 n))])
      (printf "  [~a]: ~a\n" i (vector-ref nested i)))
    (printf "\nFlattened result: ~a\n" seq-result)))
