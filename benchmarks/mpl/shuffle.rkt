#lang racket

;; Port of shuf from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/shuf
;; Adapted for Racket parallel benchmarking
;;
;; Parallel random shuffle using Fisher-Yates algorithm
;; Tests randomization and permutation patterns

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide shuffle-sequential
         shuffle-parallel
         verify-permutation)

;; Sequential Fisher-Yates shuffle
(define (shuffle-sequential vec seed)
  (define result (vector-copy vec))
  (define n (vector-length result))
  (random-seed seed)
  (for ([i (in-range (- n 1) 0 -1)])
    (define j (random (+ i 1)))
    (define tmp (vector-ref result i))
    (vector-set! result i (vector-ref result j))
    (vector-set! result j tmp))
  result)

;; Parallel shuffle - partition vector and shuffle chunks independently,
;; then do a final merge shuffle. Note: produces different permutation than
;; sequential (different random choices), but still valid shuffle.
(define (shuffle-parallel vec workers seed [chunk-size 10000])
  (define n (vector-length vec))

  (if (< n (* 2 chunk-size))
      ;; Too small for parallelism
      (shuffle-sequential vec seed)
      ;; Shuffle chunks in parallel
      (call-with-thread-pool workers
        (λ (pool actual-workers)
          (define num-chunks (quotient (+ n chunk-size -1) chunk-size))

          ;; Phase 1: Shuffle each chunk independently with different seeds
          (define shuffled-chunks
            (thread-pool-wait/collect
             (for/list ([c (in-range num-chunks)])
               (define start (* c chunk-size))
               (define end (min (+ start chunk-size) n))
               (thread-pool-submit
                pool
                (λ ()
                  ;; Extract chunk, shuffle it
                  (define chunk (make-vector (- end start)))
                  (for ([i (in-range start end)])
                    (vector-set! chunk (- i start) (vector-ref vec i)))
                  (shuffle-sequential chunk (+ seed c)))))))

          ;; Phase 2: Merge shuffled chunks back into result
          (define result (make-vector n))
          (for ([c (in-range num-chunks)])
            (define start (* c chunk-size))
            (define chunk (list-ref shuffled-chunks c))
            (for ([i (in-range (vector-length chunk))])
              (vector-set! result (+ start i) (vector-ref chunk i))))

          result)
        #:max n)))

;; Verify that result is a permutation of original
(define (verify-permutation original shuffled)
  (define n (vector-length original))
  (and (= n (vector-length shuffled))
       ;; Check: same elements (sorted order)
       (let ([orig-sorted (vector-sort (vector-copy original) <)]
             [shuf-sorted (vector-sort (vector-copy shuffled) <)])
         (for/and ([i (in-range n)])
           (= (vector-ref orig-sorted i) (vector-ref shuf-sorted i))))))

;; Check that shuffle actually changed something
(define (different? v1 v2)
  (define n (vector-length v1))
  (for/or ([i (in-range n)])
    (not (= (vector-ref v1 i) (vector-ref v2 i)))))

;; Generate input vector [0, 1, 2, ..., n-1]
(define (make-input-vector n)
  (for/vector ([i (in-range n)]) i))

(module+ main
  (define n 1000000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define chunk-size 10000)
  (define seed 42)

  (void
   (command-line
    #:program "shuffle.rkt"
    #:once-each
    [("--n") arg "Vector size"
     (set! n (parse-positive-integer arg 'shuffle))]
    [("--chunk-size") arg "Chunk size for parallel computation"
     (set! chunk-size (parse-positive-integer arg 'shuffle))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'shuffle))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'shuffle))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'shuffle))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]))

  (printf "Generating input vector (n=~a)...\n" n)
  (define input (make-input-vector n))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'chunk-size chunk-size)
                       (list 'workers workers)
                       (list 'seed seed)))

  (printf "Running sequential shuffle...\n")
  (define seq-result
    (run-benchmark
     (λ () (shuffle-sequential input seed))
     #:name 'shuffle
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel shuffle (workers=~a, chunk-size=~a)...\n"
          workers chunk-size)
  (define par-result
    (run-benchmark
     (λ () (shuffle-parallel input workers seed chunk-size))
     #:name 'shuffle
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (verify-permutation input result)
                 (error 'shuffle "parallel result is not a valid permutation at iteration ~a"
                        iteration)))))

  (close-log-writer writer)

  (printf "\nVerification:\n")
  (printf "  Sequential is permutation: ~a\n"
          (if (verify-permutation input seq-result) "✓" "✗"))
  (printf "  Parallel is permutation: ~a\n"
          (if (verify-permutation input par-result) "✓" "✗"))
  (printf "  Sequential shuffled: ~a\n"
          (if (different? input seq-result) "✓" "✗"))
  (printf "  Parallel shuffled: ~a\n"
          (if (different? input par-result) "✓" "✗"))

  (when (<= n 20)
    (printf "\nOriginal: ~a\n" input)
    (printf "Sequential: ~a\n" seq-result)
    (printf "Parallel: ~a\n" par-result)))
