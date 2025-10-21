#lang racket

(require racket/list
         "../common/parallel.rkt"
         "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "dna-utils.rkt")

(provide fasta)

;; LCG parameters shared with dna-utils
(define lcg-modulus 139968)
(define lcg-multiplier 3877)
(define lcg-increment 29573)

(define (lcg-next-state state)
  (modulo (+ (* state lcg-multiplier) lcg-increment) lcg-modulus))

(define (lcg-transform steps)
  (let loop ([k steps]
             [mul 1]
             [add 0]
             [base-mul lcg-multiplier]
             [base-add lcg-increment])
    (cond
      [(zero? k) (values mul add)]
      [(even? k)
       (define new-base-mul (modulo (* base-mul base-mul) lcg-modulus))
       (define new-base-add (modulo (+ (* base-mul base-add) base-add) lcg-modulus))
       (loop (quotient k 2) mul add new-base-mul new-base-add)]
      [else
       (define new-mul (modulo (* base-mul mul) lcg-modulus))
       (define new-add (modulo (+ (* base-mul add) base-add) lcg-modulus))
       (loop (sub1 k) new-mul new-add base-mul base-add)])))

(define (make-ranges total chunk-size)
  (if (or (zero? total) (<= chunk-size 0))
      '()
      (for/list ([start (in-range 0 total chunk-size)])
        (cons start (min total (+ start chunk-size))))))

(define (copy-chunk! target start chunk)
  (define len (string-length chunk))
  (for ([i (in-range len)])
    (string-set! target (+ start i) (string-ref chunk i))))

(define (parallel-repeat-sequence total-length base pool workers)
  (cond
    [(or (<= workers 1) (<= total-length 0))
     (generate-repeat-sequence total-length base)]
    [else
     (define base-length (string-length base))
     (define chunk-size (max 1 (ceiling (/ total-length workers))))
     (define ranges (make-ranges total-length chunk-size))
     (define tasks
       (for/list ([rg (in-list ranges)])
         (define start (car rg))
         (define end (cdr rg))
         (thread-pool-submit
          pool
          (λ ()
            (define len (- end start))
            (define chunk (make-string len))
            (for ([i (in-range len)])
              (define ch (string-ref base (modulo (+ start i) base-length)))
               (string-set! chunk i ch))
             chunk))))
     (define substrings (thread-pool-wait/collect tasks))
     (define result (make-string total-length))
     (for ([rg (in-list ranges)]
           [chunk (in-list substrings)])
       (copy-chunk! result (car rg) chunk))
     result]))

(define (cdf-select value cdf fallback)
  (let loop ([entries cdf] [default fallback])
    (cond
      [(null? entries) default]
      [(< value (caar entries)) (cdar entries)]
      [else (loop (cdr entries) default)])))

(define (parallel-random-sequence total-length freqs seed pool workers)
  (if (or (<= workers 1) (<= total-length 0))
      (let ([rng (make-lcg seed)])
        (define str (generate-random-sequence total-length freqs rng))
        (define state seed)
        (for ([_ (in-range total-length)])
          (set! state (lcg-next-state state)))
        (values str state))
      (let* ([cdf (build-cdf freqs)]
             [fallback (cdr (last cdf))]
             [chunk-size (max 1 (ceiling (/ total-length workers)))]
             [ranges (make-ranges total-length chunk-size)]
             [chunk-count (length ranges)])
        (define states (make-vector chunk-count 0))
        (define state seed)
        (for ([idx (in-range chunk-count)]
              [rg (in-list ranges)])
          (vector-set! states idx state)
          (define len (- (cdr rg) (car rg)))
          (define-values (mul add) (lcg-transform len))
          (set! state (modulo (+ (* mul state) add) lcg-modulus)))
        (define tasks
          (for/list ([rg (in-list ranges)]
                     [idx (in-naturals)])
            (define start-state (vector-ref states idx))
            (define len (- (cdr rg) (car rg)))
            (thread-pool-submit
             pool
             (λ ()
               (define chunk (make-string len))
               (define local-state start-state)
               (for ([i (in-range len)])
                 (set! local-state (lcg-next-state local-state))
                 (define ch (cdf-select (/ local-state lcg-modulus) cdf fallback))
                 (string-set! chunk i ch))
             chunk))))
        (define substrings (thread-pool-wait/collect tasks))
        (define result (make-string total-length))
        (for ([rg (in-list ranges)]
              [chunk (in-list substrings)])
          (copy-chunk! result (car rg) chunk))
        (values result state))))

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
  (if (<= workers 1)
      (fasta-sequential n)
      (call-with-thread-pool workers
        (λ (pool actual-workers)
          (call-with-values
              make-sink
            (λ (emit-byte emit-string emit-substring emit-newline snapshot)
              (define effective-workers (max 1 actual-workers))
              ;; Repeat sequence
              (emit-string ">ONE Homo sapiens alu")
              (emit-newline)
              (define repeat-str
                (parallel-repeat-sequence (* 2 n) alu-sequence pool effective-workers))
              (emit-lines repeat-str emit-substring emit-newline)
              ;; Random sequences (IUB then Homo sapiens) sharing RNG state
              (define rng-state 42)
              (emit-string ">TWO IUB ambiguity codes")
              (emit-newline)
              (define-values (iub-str state-after-iub)
                (parallel-random-sequence (* 3 n) iub-frequencies rng-state pool effective-workers))
              (emit-lines iub-str emit-substring emit-newline)
              (emit-string ">THREE Homo sapiens frequency")
              (emit-newline)
              (define-values (human-str _final-state)
                (parallel-random-sequence (* 5 n) homo-sapiens-frequencies state-after-iub pool effective-workers))
              (emit-lines human-str emit-substring emit-newline)
              (snapshot))))
        #:max (max 1 (* 3 workers)))))

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
