#lang racket

(require racket/cmdline
         racket/path
         racket/file
         racket/system
         racket/string
         "common/cli.rkt"
         "common/parallel.rkt")

(provide run-suite)

;; Suite definitions mapping suite names to benchmark configurations
(define racket-benchmarks
  '((bmbench "racket/bmbench.rkt" (--sizes "20000" --workers "4" --repeat "3"))
    (bmbench-improved "racket/bmbench_improved.rkt" (--sizes "20000" --workers "4" --repeat "3"))
    (richards "racket/richards.rkt" (--iterations "5" --workers "4" --repeat "3"))
    (rows1b "racket/rows1b.rkt" (--rows "1000000" --workers "4" --chunk-size "100000" --repeat "3"))))

(define shootout-benchmarks
  '((spectral-norm "shootout/spectral-norm.rkt" (--n "1000" --iterations "10" --workers "4" --repeat "3"))
    (binary-trees "shootout/binary-trees.rkt" (--max-depth "14" --workers "4" --repeat "3"))
    (nbody "shootout/nbody.rkt" (--n "100000" --workers "4" --repeat "3"))
    (fannkuch-redux "shootout/fannkuch-redux.rkt" (--n "9" --workers "1" --repeat "3"))
    (mandelbrot "shootout/mandelbrot.rkt" (--n "500" --workers "4" --repeat "3"))
    (fasta "shootout/fasta.rkt" (--n "100000" --workers "4" --repeat "3"))
    (regex-dna "shootout/regex-dna.rkt" (--n "100000" --workers "4" --repeat "3"))
    (k-nucleotide "shootout/k-nucleotide.rkt" (--n "100000" --workers "4" --repeat "3"))))

(define nas-benchmarks
  '((nas-ep "nas/ep.rkt" (--class "S" --workers "4" --repeat "3"))))

(define fib-scaling-benchmarks
  (for/list ([w (in-range 1 9)])
    (list
     (string->symbol (format "fib-workers-~a" w))
     "mpl/fib.rkt"
     (list '--n "42"
           '--threshold "20"
           '--workers (format "~a" w)
           '--repeat "3"))))

(define merge-sort-scaling-benchmarks
  (for/list ([w (in-range 1 9)])
    (list
     (string->symbol (format "merge-sort-workers-~a" w))
     "mpl/merge-sort.rkt"
     (list '--n "1000000"
           '--seed "42"
           '--threshold "2000"
           '--workers (format "~a" w)
           '--repeat "3"))))

(define mpl-benchmarks
  '((histogram "mpl/histogram.rkt" (--n "1000000" --buckets "256" --workers "4" --repeat "3"))
    (integer-sort "mpl/integer-sort.rkt" (--n "1000000" --range "100000" --workers "4" --repeat "3"))
    (bfs "mpl/bfs.rkt" (--n "10000" --edge-prob "0.001" --source "0" --workers "4" --repeat "3"))
    (mis "mpl/mis.rkt" (--n "5000" --degree "10" --workers "4" --repeat "3"))
    (msf "mpl/msf.rkt" (--n "1000" --degree "10" --workers "4" --repeat "3"))
    (suffix-array "mpl/suffix-array.rkt" (--n "50000" --alphabet "4" --workers "4" --repeat "3"))
    (convex-hull "mpl/convex-hull.rkt" (--n "10000" --distribution "uniform-circle" --workers "4" --repeat "3"))))

(define toy-benchmarks
  '((fib "mpl/fib.rkt" (--n "38" --threshold "22" --repeat "5"))
    (bignum-add "mpl/bignum-add.rkt" (--n "1000000" --chunk-size "100000" --repeat "5" --seed "42"))
    (palindrome "mpl/palindrome.rkt" (--n "20000000" --chunk-size "500000" --repeat "5" --seed "42" --palindrome))
    (shuffle "mpl/shuffle.rkt" (--n "2000000" --chunk-size "200000" --repeat "5" --seed "42"))
    (flatten "mpl/flatten.rkt" (--n "20000" --avg-size "200" --repeat "5" --seed "42"))
    (collect "mpl/collect.rkt" (--n "5000000" --predicate "gt-50" --repeat "5" --seed "42"))))

;; Get benchmark suite by name
(define (get-suite name)
  (case (string->symbol name)
    [(racket) racket-benchmarks]
    [(shootout) shootout-benchmarks]
    [(nas) nas-benchmarks]
    [(fib-scaling) fib-scaling-benchmarks]
    [(merge-sort-scaling) merge-sort-scaling-benchmarks]
    [(mpl) mpl-benchmarks]
    [(toy) toy-benchmarks]
    [(all) (append racket-benchmarks shootout-benchmarks nas-benchmarks mpl-benchmarks)]
    [else (error 'run-suite "unknown suite: ~a" name)]))

;; Load configuration from S-expression file
(define (load-config config-path)
  (define sexp (call-with-input-file config-path read))
  ;; Convert association list to hash
  (for/hash ([entry sexp])
    (values (car entry) (cdr entry))))

;; Apply configuration overrides to benchmark specs
(define (apply-config-overrides benchmarks config)
  (define overrides (cdar (hash-ref config 'overrides '())))
  (for/list ([bench benchmarks])
    (match-define (list name path args) bench)
    (define override (assoc name overrides))
    (if override
        (list name path (cadr override))
        bench)))

(define (override-workers args worker)
  (define worker-str (number->string worker))
  (define (loop remaining acc replaced?)
    (cond
      [(null? remaining)
       (if replaced?
           (reverse acc)
           (reverse (cons worker-str (cons '--workers acc))))]
      [(and (symbol? (car remaining))
            (eq? '--workers (car remaining))
            (pair? (cdr remaining)))
       (loop (cddr remaining)
             (cons worker-str (cons '--workers acc))
             #t)]
      [else
       (loop (cdr remaining)
             (cons (car remaining) acc)
             replaced?)]))
  (loop args '() #f))

;; Run a single benchmark
(define (run-benchmark name path args log-dir benchmarks-dir worker skip-seq-log?)
  (define full-path (build-path benchmarks-dir path))
  (define log-suffix (if worker (format "-w~a" worker) ""))
  (define log-file (build-path log-dir (format "~a~a.sexp" name log-suffix)))
  (define base-args (if worker (override-workers args worker) args))
  (define extended-args (if skip-seq-log?
                            (append base-args (list '--skip-sequential-log))
                            base-args))
  (define all-args (append (map (λ (a) (if (symbol? a) (symbol->string a) a)) extended-args)
                           (list "--log" (path->string log-file))))

  (printf "Running ~a...\n" name)
  (define-values (proc out in err)
    (apply subprocess #f #f #f (find-executable-path "racket") (path->string full-path) all-args))
  (subprocess-wait proc)
  (define exit-code (subprocess-status proc))

  (close-output-port in)
  (close-input-port out)
  (close-input-port err)

  (if (zero? exit-code)
      (printf "  ✓ ~a completed\n" name)
      (printf "  ✗ ~a failed (exit code: ~a)\n" name exit-code))

  exit-code)

;; Run benchmark suite
(define (run-suite suite-names
                   #:config [config-path #f]
                   #:log-dir [log-dir "logs"]
                   #:parallel? [parallel? #f]
                   #:worker-counts [worker-counts '()])
  (define benchmarks-dir (build-path (current-directory) "benchmarks"))

  ;; Ensure log directory exists
  (make-directory* log-dir)

  ;; Load configuration if provided
  (define config (if config-path (load-config config-path) (hash)))

  ;; Gather all benchmarks from requested suites and apply overrides
  (define base-benchmarks
    (let ([benchmarks (apply append (map get-suite suite-names))])
      (if (hash-empty? config)
          benchmarks
          (apply-config-overrides benchmarks config))))

  (define final-benchmarks
    (if (null? worker-counts)
        (for/list ([bench base-benchmarks])
          (match-define (list name path args) bench)
          (list name path args #f #f))
        (let ([base-worker (first worker-counts)])
          (for*/list ([bench base-benchmarks]
                      [worker worker-counts])
            (match-let ([(list name path args) bench])
              (list name path args worker (not (equal? worker base-worker))))))))

  (printf "Running ~a benchmark(s) from suite(s): ~a\n"
          (length final-benchmarks)
          (string-join suite-names ", "))
  (printf "Logging to: ~a\n\n" log-dir)

  (define (run-one bench)
    (match-define (list name path args worker skip?) bench)
    (run-benchmark name path args log-dir benchmarks-dir worker skip?))

  (define results
    (if parallel?
        (call-with-thread-pool (processor-count)
          (λ (pool actual-workers)
            (define tasks (for/list ([bench final-benchmarks])
                            (thread-pool-submit pool (λ () (run-one bench)))))
            (define raw-results (thread-pool-wait/collect tasks))
            (for/list ([vals (in-list raw-results)])
              (cond
                [(null? vals) 0]
                [(null? (cdr vals)) (car vals)]
                [else (car vals)])))
          #:max (max 1 (length final-benchmarks)))
        (for/list ([bench final-benchmarks])
          (run-one bench))))

  (define failures (filter (λ (r) (not (zero? r))) results))
  (printf "\n")
  (printf "========================================\n")
  (printf "Summary: ~a/~a benchmarks completed successfully\n"
          (- (length results) (length failures))
          (length results))
  (when (not (null? failures))
    (printf "Failed benchmarks: ~a\n" (length failures)))
  (printf "========================================\n")

  (if (null? failures) 0 1))

(module+ main
  (define suites '())
  (define config-path #f)
  (define log-dir "logs")
  (define parallel? #f)
  (define worker-counts '())

  (void
   (command-line
    #:program "run-suite.rkt"
    #:once-each
    [("--suite" "-s") suite "Suite to run: racket, shootout, nas, mpl, toy, or all"
     (set! suites (cons suite suites))]
    [("--config" "-c") path "Configuration file (S-expression)"
     (set! config-path path)]
    [("--log-dir" "-l") dir "Directory for log files (default: logs)"
     (set! log-dir dir)]
    [("--parallel" "-p") "Run benchmarks in parallel (experimental)"
     (set! parallel? #t)]
    [("--worker-counts") counts "Comma-separated worker counts for parallel variants"
     (set! worker-counts
           (for/list ([part (in-list (string-split counts ","))]
                      #:when (not (string=? "" (string-trim part))))
             (string->number (string-trim part))))] 
    #:args ()
    (void)))

  (when (null? suites)
    (set! suites '("all")))

  (define exit-code (run-suite (reverse suites)
                                #:config config-path
                                #:log-dir log-dir
                                #:parallel? parallel?
                                #:worker-counts worker-counts))
  (exit exit-code))
