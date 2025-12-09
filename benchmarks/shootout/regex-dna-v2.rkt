#lang racket

(require racket/async-channel
         racket/match
         racket/string
         "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "dna-utils.rkt")

(provide regex-dna)

(define variant-patterns
  (map string-upcase
       '("agggtaaa|tttaccct"
         "[cgt]gggtaaa|tttaccc[acg]"
         "a[act]ggtaaa|tttacc[agt]t"
         "ag[act]gtaaa|tttac[agt]ct"
         "agg[act]taaa|ttta[agt]cct"
         "aggg[acg]aaa|ttt[cgt]ccct"
         "agggt[cgt]aa|tt[acg]accct"
         "agggta[cgt]a|t[acg]taccct"
         "agggtaa[cgt]|[acg]ttaccct"
         "agggtaac|gttaccct")))

(define substitutions
  (map (λ (entry) (cons (string-upcase (car entry)) (string-upcase (cdr entry))))
       '(("B" . "(c|g|t)")
         ("D" . "(a|g|t)")
         ("H" . "(a|c|t)")
         ("K" . "(g|t)")
         ("M" . "(a|c)")
         ("N" . "(a|c|g|t)")
         ("R" . "(a|g)")
         ("S" . "(c|g)")
         ("V" . "(a|c|g)")
         ("W" . "(a|t)")
         ("Y" . "(c|t)"))))

;; SBCL pattern: Phase 1 - parallel regex matching (9 patterns independently)
(define (count-variants dna workers)
  (define compiled (for/list ([pattern (in-list variant-patterns)])
                     (cons pattern (pregexp pattern))))
  (define (count-matches rx)
    (length (regexp-match* rx dna)))
  (if (<= workers 1)
      (for/list ([entry (in-list compiled)])
        (cons (car entry) (count-matches (cdr entry))))
      (let ([worker-count workers]  ; Use processor-count
            [pool (make-parallel-thread-pool workers)]
            [results (make-vector (length compiled))])
        (define threads
          (for/list ([entry (in-list compiled)]
                     [idx (in-naturals)])
            (define pattern (car entry))
            (define rx (cdr entry))
            (thread #:pool pool
                    #:keep 'results
                    (λ ()
                      (vector-set! results idx (cons pattern (count-matches rx)))))))
        (for ([t (in-list threads)]) (thread-wait t))
        (parallel-thread-pool-close pool)
        (vector->list results))))

;; SBCL pattern: Phase 2 - parallel string replacement
;; In SBCL this is done in parallel; here we parallelize across substitution patterns
(define (apply-substitutions dna workers)
  (if (<= workers 1)
      (for/fold ([str dna]) ([entry (in-list substitutions)])
        (regexp-replace* (pregexp (car entry)) str (cdr entry)))
      ;; Parallel version: each worker processes a subset of substitutions
      ;; Note: This is different from SBCL's chunked approach but achieves parallelism
      (let* ([worker-count workers]
             [pool (make-parallel-thread-pool worker-count)]
             [chunk-size (quotient (+ (length substitutions) worker-count -1) worker-count)]
             [chunks (for/list ([start (in-range 0 (length substitutions) chunk-size)])
                       (take (drop substitutions start)
                             (min chunk-size (- (length substitutions) start))))]
             [threads
              (for/list ([chunk (in-list chunks)])
                (thread #:pool pool
                        #:keep 'results
                        (λ ()
                          (for/fold ([str dna]) ([entry (in-list chunk)])
                            (regexp-replace* (pregexp (car entry)) str (cdr entry))))))])
        ;; Merge results: apply remaining substitutions sequentially
        ;; This is a simplification; true parallel replacement would need chunking the string
        (define results (map thread-wait threads))
        (parallel-thread-pool-close pool)
        ;; Return the result from the last chunk (sequential fallback for correctness)
        (for/fold ([str dna]) ([entry (in-list substitutions)])
          (regexp-replace* (pregexp (car entry)) str (cdr entry))))))

(define (regex-dna n #:workers [workers 1])
  (define dna (build-dna-sample n))
  ;; Phase 1: Parallel regex matching
  (define variant-counts (count-variants dna workers))
  ;; Phase 2: Sequential substitution (parallel version doesn't compose correctly)
  ;; SBCL does this in parallel by chunking the string; we keep it sequential for correctness
  (define substituted (apply-substitutions dna 1))
  (list variant-counts
        (string-length dna)
        (string-length substituted)))

(module+ main
  (define n 250000)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "regex-dna-v2.rkt"
    #:once-each
    [("--n") arg "Base sequence length multiplier"
     (set! n (parse-positive-integer arg 'regex-dna))]
    [("--workers") arg "Parallel worker count (SBCL uses processor-count)"
     (set! workers (parse-positive-integer arg 'regex-dna))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'regex-dna))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n) (list 'workers workers)))

  (unless skip-sequential
    (run-benchmark
     (λ () (regex-dna n #:workers 1))
     #:name 'regex-dna-v2
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (regex-dna n #:workers workers))
   #:name 'regex-dna-v2
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata)

  (close-log-writer writer))
