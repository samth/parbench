#lang racket

(require racket/async-channel
         racket/list
         "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "dna-utils.rkt")

(provide k-nucleotide)

(define target-substrings
  '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT"))

(define (frequency-table dna k)
  (define len (string-length dna))
  (if (< len k)
      (make-hash)
      (let ([table (make-hash)])
        (for ([i (in-range 0 (add1 (- len k)))])
          (define key (substring dna i (+ i k)))
          (hash-update! table key add1 0))
        table)))

(define (frequency-summary table)
  (define total (for/sum ([(key count) (in-hash table)]) count))
  (define entries
    (sort (for/list ([(key count) (in-hash table)])
            (cons key count))
          (λ (a b)
            (if (= (cdr a) (cdr b))
                (string<? (car a) (car b))
                (> (cdr a) (cdr b))))))
  (list total entries))

(define (compute-tables dna ks workers)
  (if (<= workers 1)
      (for/hash ([k (in-list ks)])
        (values k (frequency-table dna k)))
      (let* ([size (length ks)]
             [results (make-vector size)]
             [pool (make-parallel-thread-pool workers)]
             [threads
              (for/list ([k (in-list ks)]
                         [idx (in-naturals)])
                (thread
                 #:pool pool
                 #:keep 'results
                 (λ ()
                   (vector-set! results idx (cons k (frequency-table dna k))))))])
        (for ([t (in-list threads)]) (thread-wait t))
        (parallel-thread-pool-close pool)
        (for/hash ([entry (in-vector results)])
          (values (car entry) (cdr entry))))))

(define (k-nucleotide n #:workers [workers 1])
  (define dna (build-dna-sample n))
  (define ks '(1 2 3 4 6 12 18))
  (define tables (compute-tables dna ks workers))
  (define freq1 (frequency-summary (hash-ref tables 1)))
  (define freq2 (frequency-summary (hash-ref tables 2)))
  (define counts
    (for/list ([pattern (in-list target-substrings)])
      (define table (hash-ref tables (string-length pattern)))
      (list pattern (hash-ref table pattern 0))))
  (list freq1 freq2 counts))

(module+ main
  (define n 250000)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "k-nucleotide.rkt"
    #:once-each
    [("--n") arg "Base sequence length multiplier"
     (set! n (parse-positive-integer arg 'k-nucleotide))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'k-nucleotide))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'k-nucleotide))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n) (list 'workers workers)))

  (unless skip-sequential
    (run-benchmark
     (λ () (k-nucleotide n #:workers 1))
     #:name 'k-nucleotide
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (k-nucleotide n #:workers workers))
   #:name 'k-nucleotide
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata)

  (close-log-writer writer))
