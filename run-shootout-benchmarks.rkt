#lang racket

(require racket/cmdline
         racket/file
         racket/path
         racket/system
         racket/list
         json
         "benchmarks/tools/analysis.rkt")

;; Benchmark configurations: (name args...)
;; Tuned so sequential version takes >= 1 second
;; Includes both original (v1) and SBCL-style (v2) parallel implementations
(define benchmark-configs
  '((binary-trees "--n" "18" "--repeat" "3")
    (binary-trees-v2 "--n" "18" "--repeat" "3")
    (spectral-norm "--n" "3000" "--repeat" "3")
    (spectral-norm-v2 "--n" "3000" "--repeat" "3")
    (fannkuch-redux "--n" "11" "--repeat" "3")
    (fannkuch-redux-v2 "--n" "11" "--repeat" "3")
    (mandelbrot "--n" "4000" "--repeat" "3")
    (mandelbrot-v2 "--n" "4000" "--repeat" "3")
    (k-nucleotide "--n" "500000" "--repeat" "3")
    (k-nucleotide-v2 "--n" "500000" "--repeat" "3")
    (regex-dna "--n" "500000" "--repeat" "3")
    (regex-dna-v2 "--n" "500000" "--repeat" "3")))

;; All benchmarks now have sequential/parallel variants

(define worker-counts '(1 2 4 6 8))

(define (run-benchmark name args worker-count log-path)
  (define benchmark-path
    (build-path "benchmarks" "shootout" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" (number->string worker-count)
                      "--log" log-path)))
  (printf "Running ~a with ~a workers...\n" name worker-count)
  (define result (apply system* (find-executable-path "racket") benchmark-path cmd-args))
  (unless result
    (eprintf "Warning: ~a with ~a workers failed\n" name worker-count)))

(define (run-benchmark-sequential name args log-path)
  (define benchmark-path
    (build-path "benchmarks" "shootout" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" "1" "--log" log-path)))
  (printf "Running ~a sequential (workers=1)...\n" name)
  (define result (apply system* (find-executable-path "racket") benchmark-path cmd-args))
  (unless result
    (eprintf "Warning: ~a sequential failed\n" name)))

(define (run-benchmark-parallel name args worker-count log-path)
  (define benchmark-path
    (build-path "benchmarks" "shootout" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" (number->string worker-count)
                      "--log" log-path
                      "--skip-sequential")))
  (printf "Running ~a with ~a workers (parallel only)...\n" name worker-count)
  (define result (apply system* (find-executable-path "racket") benchmark-path cmd-args))
  (unless result
    (eprintf "Warning: ~a with ~a workers failed\n" name worker-count)))

(define (run-all-benchmarks log-dir)
  (make-directory* log-dir)

  (for ([config benchmark-configs])
    (define name (first config))
    (define args (rest config))
    (define log-path (build-path log-dir (format "~a.sexp" name)))

    (printf "\n=== Running ~a ===\n" name)
    (when (file-exists? log-path)
      (delete-file log-path))

    ;; Run sequential version once with workers=1
    (run-benchmark-sequential name args log-path)
    ;; Run parallel versions for all worker counts
    (for ([workers worker-counts])
      (run-benchmark-parallel name args workers log-path))))

(define (generate-html-report log-dir output-file)
  (printf "\nGenerating HTML report...\n")

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

  (printf "Report generated: ~a\n" output-file))

(module+ main
  (define log-dir "logs/shootout")
  (define output-file "shootout-results.html")

  (command-line
   #:program "run-shootout-benchmarks"
   #:once-each
   [("--log-dir") dir "Directory for log files" (set! log-dir dir)]
   [("--output") file "Output HTML file" (set! output-file file)])

  (printf "Starting shootout benchmark suite...\n")
  (printf "Worker counts: ~a\n" worker-counts)
  (printf "Log directory: ~a\n" log-dir)
  (printf "Output file: ~a\n\n" output-file)

  (run-all-benchmarks log-dir)
  (generate-html-report log-dir output-file)

  (printf "\nDone! Open ~a in your browser to view results.\n" output-file))
