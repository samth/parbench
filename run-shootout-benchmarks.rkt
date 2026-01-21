#lang racket

(require racket/cmdline
         racket/file
         racket/path
         racket/system
         racket/list
         racket/string
         json
         "benchmarks/tools/analysis.rkt")

;; Benchmark configurations: (name args...)
;; Tuned so sequential version takes >= 1 second
;; Each benchmark has built-in sequential and parallel variants
(define benchmark-configs
  '((binary-trees "--n" "18" "--repeat" "10")
    (spectral-norm "--n" "3000" "--repeat" "10")
    (fannkuch-redux "--n" "11" "--repeat" "10")
    (mandelbrot "--n" "4000" "--repeat" "10")
    (k-nucleotide "--n" "500000" "--repeat" "10")
    (regex-dna "--n" "500000" "--repeat" "10")))

;; All benchmarks now have sequential/parallel variants

(define worker-counts '(1 2 4 6 8))

;; Quiet mode suppresses per-benchmark output
(define quiet-mode (make-parameter #f))

;; Parse comma-separated worker counts string into list of numbers
(define (parse-worker-counts str)
  (map string->number (string-split str ",")))

(define (run-benchmark name args worker-count log-path)
  (define benchmark-path
    (build-path "benchmarks" "shootout" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" (number->string worker-count)
                      "--log" log-path)))
  (unless (quiet-mode)
    (printf "  ~a (workers=~a)..." name worker-count)
    (flush-output))
  (define result
    (if (quiet-mode)
        (parameterize ([current-output-port (open-output-nowhere)])
          (apply system* (find-system-path 'exec-file) benchmark-path cmd-args))
        (apply system* (find-system-path 'exec-file) benchmark-path cmd-args)))
  (unless (quiet-mode)
    (if result (printf " done\n") (printf " FAILED\n")))
  (unless result
    (eprintf "Warning: ~a with ~a workers failed\n" name worker-count)))

(define (run-benchmark-sequential name args log-path)
  (define benchmark-path
    (build-path "benchmarks" "shootout" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" "1" "--log" log-path)))
  (unless (quiet-mode)
    (printf "  ~a (sequential)..." name)
    (flush-output))
  (define result
    (if (quiet-mode)
        (parameterize ([current-output-port (open-output-nowhere)])
          (apply system* (find-system-path 'exec-file) benchmark-path cmd-args))
        (apply system* (find-system-path 'exec-file) benchmark-path cmd-args)))
  (unless (quiet-mode)
    (if result (printf " done\n") (printf " FAILED\n")))
  (unless result
    (eprintf "Warning: ~a sequential failed\n" name)))

(define (run-benchmark-parallel name args worker-count log-path)
  (define benchmark-path
    (build-path "benchmarks" "shootout" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" (number->string worker-count)
                      "--log" log-path
                      "--skip-sequential")))
  (unless (quiet-mode)
    (printf "  ~a (workers=~a)..." name worker-count)
    (flush-output))
  (define result
    (if (quiet-mode)
        (parameterize ([current-output-port (open-output-nowhere)])
          (apply system* (find-system-path 'exec-file) benchmark-path cmd-args))
        (apply system* (find-system-path 'exec-file) benchmark-path cmd-args)))
  (unless (quiet-mode)
    (if result (printf " done\n") (printf " FAILED\n")))
  (unless result
    (eprintf "Warning: ~a with ~a workers failed\n" name worker-count)))

(define (run-all-benchmarks log-dir)
  (run-all-benchmarks-with-workers log-dir worker-counts))

(define (run-all-benchmarks-with-workers log-dir workers-list)
  (make-directory* log-dir)
  (define total (length benchmark-configs))
  (define current 0)

  (for ([config benchmark-configs])
    (define name (first config))
    (define args (rest config))
    (define log-path (build-path log-dir (format "~a.sexp" name)))

    (set! current (add1 current))
    (when (quiet-mode)
      (printf "  [~a/~a] ~a\n" current total name)
      (flush-output))
    (unless (quiet-mode)
      (printf "\n=== Running ~a ===\n" name))
    (when (file-exists? log-path)
      (delete-file log-path))

    ;; Run sequential version once with workers=1
    (run-benchmark-sequential name args log-path)
    ;; Run parallel versions for all worker counts
    (for ([workers workers-list])
      (run-benchmark-parallel name args workers log-path))))

(define (generate-html-report log-dir output-file)
  (unless (quiet-mode)
    (printf "\nGenerating HTML report...\n"))

  ;; Read all log files
  (define log-files
    (for/list ([f (in-directory log-dir)]
               #:when (equal? (path-get-extension f) #".sexp"))
      f))

  (define summaries (load-summaries (map path->string log-files)))

  ;; Generate JSON data
  (define json-data
    (jsexpr->string
     (hasheq
      'benchmarks
      (for/list ([bench-summaries (group-by summary-name summaries)])
        (define bench-name (summary-name (first bench-summaries)))
        (hasheq
         'name (symbol->string bench-name)
         'variants
         (for/list ([s bench-summaries])
           (hasheq
            'variant (symbol->string (summary-variant s))
            'worker (summary-worker s)
            'count (summary-count s)
            'real_mean (summary-real-mean s)
            'real_stddev (summary-real-stddev s)
            'real_min (summary-real-min s)
            'real_max (summary-real-max s)
            'cpu_mean (summary-cpu-mean s)
            'gc_mean (summary-gc-mean s))))))))

  ;; Read HTML template
  (define template-path
    (build-path "benchmarks" "tools" "templates" "visualization.html"))
  (define template (file->string template-path))

  ;; Replace placeholders
  (define html
    (string-replace
     (string-replace template "@@TITLE@@" "Shootout Benchmarks" #:all? #t)
     "@@DATA@@" json-data #:all? #t))

  ;; Write output
  (call-with-output-file output-file
    #:exists 'replace
    (λ (out) (write-string html out)))

  (unless (quiet-mode)
    (printf "Report generated: ~a\n" output-file)))

(module+ main
  (define log-dir "logs/shootout")
  (define output-file "shootout-results.html")
  (define custom-workers #f)
  (define quiet #f)

  (command-line
   #:program "run-shootout-benchmarks"
   #:once-each
   [("--log-dir") dir "Directory for log files" (set! log-dir dir)]
   [("--output") file "Output HTML file" (set! output-file file)]
   [("--workers") counts "Comma-separated worker counts (e.g., 1,2,4,8)"
    (set! custom-workers (parse-worker-counts counts))]
   [("--quiet" "-q") "Suppress per-benchmark output" (set! quiet #t)])

  ;; Use custom workers if provided, otherwise default
  (define active-workers (or custom-workers worker-counts))

  (parameterize ([quiet-mode quiet])
    (unless quiet
      (printf "Starting shootout benchmark suite...\n")
      (printf "Worker counts: ~a\n" active-workers)
      (printf "Log directory: ~a\n" log-dir)
      (printf "Output file: ~a\n\n" output-file))

    (run-all-benchmarks-with-workers log-dir active-workers)
    (unless (equal? output-file "/dev/null")
      (generate-html-report log-dir output-file))

    (unless quiet
      (printf "\nDone! Open ~a in your browser to view results.\n" output-file))))
