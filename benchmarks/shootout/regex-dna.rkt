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

(define (count-variants dna workers)
  (define compiled (for/list ([pattern (in-list variant-patterns)])
                     (cons pattern (pregexp pattern))))
  (define (count-matches rx)
    (length (regexp-match* rx dna)))
  (if (<= workers 1)
      (for/list ([entry (in-list compiled)])
        (cons (car entry) (count-matches (cdr entry))))
      (let ([pool (make-parallel-thread-pool workers)]
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

(define (apply-substitutions dna)
  (for/fold ([str dna]) ([entry (in-list substitutions)])
    (regexp-replace* (pregexp (car entry)) str (cdr entry))))

(define (regex-dna n #:workers [workers 1])
  (define dna (build-dna-sample n))
  (define variant-counts (count-variants dna workers))
  (define substituted (apply-substitutions dna))
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
    #:program "regex-dna.rkt"
    #:once-each
    [("--n") arg "Base sequence length multiplier"
     (set! n (parse-positive-integer arg 'regex-dna))]
    [("--workers") arg "Parallel worker count"
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
     #:name 'regex-dna
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (regex-dna n #:workers workers))
   #:name 'regex-dna
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata)

  (close-log-writer writer))
