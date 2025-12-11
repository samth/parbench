#lang racket

;; Port of collect from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/collect
;; Adapted for Racket parallel benchmarking
;;
;; Collect/filter operations - tests parallel filtering and compaction patterns

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide collect-sequential
         collect-parallel
         generate-input-vector)

;; Sequential collect - filter elements matching predicate
(define (collect-sequential vec pred)
  (define result-list '())
  (for ([x (in-vector vec)])
    (when (pred x)
      (set! result-list (cons x result-list))))
  (list->vector (reverse result-list)))

;; Parallel collect using two-phase approach:
;; 1. Parallel filter to identify matches and compute positions (prefix sum)
;; 2. Parallel copy of matches to output
(define (collect-parallel vec pred workers)
  (define n (vector-length vec))
  (define chunk-size (quotient (+ n workers -1) workers))

  ;; Phase 1: Filter in parallel, compute local counts
  (define channels1
    (for/list ([w (in-range workers)])
      (define ch (make-channel))
      (define start (* w chunk-size))
      (define end (min (+ start chunk-size) n))
      (thread
       (λ ()
         ;; Identify matches in this chunk
         (define matches '())
         (for ([i (in-range start end)])
           (define x (vector-ref vec i))
           (when (pred x)
             (set! matches (cons (cons i x) matches))))
         (channel-put ch (reverse matches))))
      ch))

  (define chunk-data (map channel-get channels1))

  ;; Phase 2: Sequential concatenation (could be parallelized with prefix sum)
  (define total-count
    (for/sum ([chunk (in-list chunk-data)])
      (length chunk)))

  ;; Phase 3: Parallel copy to result
  (define result (make-vector total-count))
  (define positions
    (let loop ([chunks chunk-data] [pos 0] [acc '()])
      (if (null? chunks)
          (reverse acc)
          (loop (cdr chunks)
                (+ pos (length (car chunks)))
                (cons (cons pos (car chunks)) acc)))))

  (define threads2
    (for/list ([pos-chunk (in-list positions)])
      (define pos (car pos-chunk))
      (define chunk (cdr pos-chunk))
      (thread
       (λ ()
         (for ([i (in-naturals)]
               [match (in-list chunk)])
           (vector-set! result (+ pos i) (cdr match)))))))

  (for-each thread-wait threads2)

  result)

;; Generate input vector with mix of values
(define (generate-input-vector n seed)
  (random-seed seed)
  (for/vector ([i (in-range n)])
    (random 100)))

;; Verify two vectors are equal
(define (vectors-equal? v1 v2)
  (and (= (vector-length v1) (vector-length v2))
       (for/and ([i (in-range (vector-length v1))])
         (= (vector-ref v1 i) (vector-ref v2 i)))))

;; Example predicates
(define (even-pred x) (even? x))
(define (div-by-3-pred x) (zero? (modulo x 3)))
(define (greater-than-50-pred x) (> x 50))

(module+ main
  (define n 10000000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define predicate-name 'even)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "collect.rkt"
    #:once-each
    [("--n") arg "Vector size"
     (set! n (parse-positive-integer arg 'collect))]
    [("--predicate") arg "Predicate: even, div-by-3, gt-50"
     (set! predicate-name (string->symbol arg))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'collect))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'collect))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'collect))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (define pred
    (case predicate-name
      [(even) even-pred]
      [(div-by-3) div-by-3-pred]
      [(gt-50) greater-than-50-pred]
      [else (error 'collect "unknown predicate: ~a" predicate-name)]))

  (printf "Generating input vector (n=~a)...\n" n)
  (define input (generate-input-vector n seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'predicate predicate-name)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential collect (predicate: ~a)...\n" predicate-name)
    (set! seq-result
      (run-benchmark
       (λ () (collect-sequential input pred))
       #:name 'collect
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel collect (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (collect-parallel input pred workers))
     #:name 'collect
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (vectors-equal? seq-result result)))
                 (error 'collect "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (vectors-equal? seq-result par-result)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "Collected ~a elements (from ~a, ~a%)\n"
          (vector-length par-result)
          n
          (~r (* 100.0 (/ (vector-length par-result) n)) #:precision 1))

  (when (<= (vector-length par-result) 50)
    (printf "\nResult: ~a\n" par-result)))
