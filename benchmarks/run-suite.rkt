#lang racket

(require racket/cmdline
         racket/path
         racket/file
         racket/system
         "common/cli.rkt")

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
    (k-nucleotide "shootout/k-nucleotide.rkt" (--n "100000" --workers "4" --repeat "3"))
    (chameneos "shootout/chameneos.rkt" (--n "5000" --repeat "3"))))

(define nas-benchmarks
  '((nas-ep "nas/ep.rkt" (--class "S" --workers "4" --repeat "3"))))

(define mpl-benchmarks
  '((histogram "mpl/histogram.rkt" (--n "1000000" --buckets "256" --workers "4" --repeat "3"))
    (integer-sort "mpl/integer-sort.rkt" (--n "1000000" --range "100000" --workers "4" --repeat "3"))
    (bfs "mpl/bfs.rkt" (--n "10000" --edge-prob "0.001" --source "0" --workers "4" --repeat "3"))
    (mis "mpl/mis.rkt" (--n "5000" --degree "10" --workers "4" --repeat "3"))
    (msf "mpl/msf.rkt" (--n "1000" --degree "10" --workers "4" --repeat "3"))
    (suffix-array "mpl/suffix-array.rkt" (--n "50000" --alphabet "4" --workers "4" --repeat "3"))
    (convex-hull "mpl/convex-hull.rkt" (--n "10000" --distribution "uniform-circle" --workers "4" --repeat "3"))))

;; Get benchmark suite by name
(define (get-suite name)
  (case (string->symbol name)
    [(racket) racket-benchmarks]
    [(shootout) shootout-benchmarks]
    [(nas) nas-benchmarks]
    [(mpl) mpl-benchmarks]
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

;; Run a single benchmark
(define (run-benchmark name path args log-dir benchmarks-dir)
  (define full-path (build-path benchmarks-dir path))
  (define log-file (build-path log-dir (format "~a.sexp" name)))
  (define all-args (append (map (λ (a) (if (symbol? a) (symbol->string a) a)) args)
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
(define (run-suite suite-names #:config [config-path #f] #:log-dir [log-dir "logs"] #:parallel? [parallel? #f])
  (define benchmarks-dir (build-path (current-directory) "benchmarks"))

  ;; Ensure log directory exists
  (make-directory* log-dir)

  ;; Load configuration if provided
  (define config (if config-path (load-config config-path) (hash)))

  ;; Gather all benchmarks from requested suites
  (define all-benchmarks
    (apply append (map get-suite suite-names)))

  ;; Apply configuration overrides
  (define final-benchmarks
    (if (hash-empty? config)
        all-benchmarks
        (apply-config-overrides all-benchmarks config)))

  (printf "Running ~a benchmark(s) from suite(s): ~a\n"
          (length final-benchmarks)
          (string-join suite-names ", "))
  (printf "Logging to: ~a\n\n" log-dir)

  ;; Run benchmarks
  (define results
    (if parallel?
        ;; Parallel execution (experimental)
        (for/list ([bench final-benchmarks])
          (match-define (list name path args) bench)
          (thread (λ () (run-benchmark name path args log-dir benchmarks-dir))))
        ;; Sequential execution
        (for/list ([bench final-benchmarks])
          (match-define (list name path args) bench)
          (run-benchmark name path args log-dir benchmarks-dir))))

  ;; Summary
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

  (void
   (command-line
    #:program "run-suite.rkt"
    #:once-each
    [("--suite" "-s") suite "Suite to run: racket, shootout, nas, mpl, or all"
     (set! suites (cons suite suites))]
    [("--config" "-c") path "Configuration file (S-expression)"
     (set! config-path path)]
    [("--log-dir" "-l") dir "Directory for log files (default: logs)"
     (set! log-dir dir)]
    [("--parallel" "-p") "Run benchmarks in parallel (experimental)"
     (set! parallel? #t)]
    #:args ()
    (void)))

  (when (null? suites)
    (set! suites '("all")))

  (define exit-code (run-suite (reverse suites)
                                #:config config-path
                                #:log-dir log-dir
                                #:parallel? parallel?))
  (exit exit-code))
