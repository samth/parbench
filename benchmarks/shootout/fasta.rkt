#lang racket

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "dna-utils.rkt")

(provide fasta)

(define (make-sink)
  (define len 0)
  (define sum 0)
  (define hash 0)
  (define modulus #x100000000)
  (define (emit-byte b)
    (set! len (add1 len))
    (set! sum (+ sum b))
    (set! hash (remainder (+ (* hash 131) b) modulus)))
  (define (emit-string s)
    (for ([ch (in-string s)])
      (emit-byte (char->integer ch))))
  (define (emit-substring s start end)
    (for ([i (in-range start end)])
      (emit-byte (char->integer (string-ref s i)))))
  (define (emit-newline)
    (emit-byte 10))
  (values emit-byte emit-string emit-substring emit-newline (λ () (list len sum hash))))

(define (emit-lines seq emit-substring emit-newline)
  (define total (string-length seq))
  (for ([start (in-range 0 total 60)])
    (define end (min total (+ start 60)))
    (emit-substring seq start end)
    (emit-newline)))

(define (repeat-fasta header sequence count emit-byte emit-string emit-substring emit-newline)
  (emit-string header)
  (emit-newline)
  (emit-lines (generate-repeat-sequence count sequence) emit-substring emit-newline))

(define (random-fasta header frequencies count rng emit-byte emit-string emit-substring emit-newline)
  (emit-string header)
  (emit-newline)
  (emit-lines (generate-random-sequence count frequencies rng) emit-substring emit-newline))

(define (fasta-sequential n)
  (define rng (make-lcg 42))
  (call-with-values
      make-sink
    (λ (emit-byte emit-string emit-substring emit-newline snapshot)
      (repeat-fasta ">ONE Homo sapiens alu" alu-sequence (* 2 n)
                    emit-byte emit-string emit-substring emit-newline)
      (random-fasta ">TWO IUB ambiguity codes" iub-frequencies (* 3 n) rng
                    emit-byte emit-string emit-substring emit-newline)
      (random-fasta ">THREE Homo sapiens frequency" homo-sapiens-frequencies (* 5 n) rng
                    emit-byte emit-string emit-substring emit-newline)
      (snapshot))))

(define (fasta n #:workers [workers 1])
  (fasta-sequential n))

(module+ main
  (define n 250000)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "fasta.rkt"
    #:once-each
    [("--n") arg "Base sequence length multiplier"
     (set! n (parse-positive-integer arg 'fasta))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'fasta))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'fasta))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n) (list 'workers workers)))

  (define sequential
    (run-benchmark
     (λ () (fasta n #:workers 1))
     #:name 'fasta
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (fasta n #:workers workers))
   #:name 'fasta
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (λ (_ value)
             (unless (equal? value sequential)
               (error 'fasta "parallel mismatch: ~a vs ~a" value sequential))))

  (close-log-writer writer))
