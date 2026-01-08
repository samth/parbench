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
;; Tuned so sequential version takes ~500ms per iteration
(define benchmark-configs
  '((bmbench "--n" "1000000" "--repeat" "10")
    (richards "--iterations" "100" "--repeat" "10")
    (rows1b "--rows" "2000000" "--chunk-size" "200000" "--repeat" "10")))

(define worker-counts '(1 2 4 6 8))

;; Parse comma-separated worker counts string into list of numbers
(define (parse-worker-counts str)
  (map string->number (string-split str ",")))

(define (run-benchmark-sequential name args log-path)
  (define benchmark-path
    (build-path "benchmarks" "racket" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" "1" "--log" log-path)))
  (printf "Running ~a sequential (workers=1)...\n" name)
  (define result (apply system* (find-executable-path "racket") benchmark-path cmd-args))
  (unless result
    (eprintf "Warning: ~a sequential failed\n" name)))

(define (run-benchmark-parallel name args worker-count log-path)
  (define benchmark-path
    (build-path "benchmarks" "racket" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" (number->string worker-count)
                       "--log" log-path
                       "--skip-sequential")))
  (printf "Running ~a with ~a workers (parallel only)...\n" name worker-count)
  (define result (apply system* (find-executable-path "racket") benchmark-path cmd-args))
  (unless result
    (eprintf "Warning: ~a with ~a workers failed\n" name worker-count)))

(define (run-all-benchmarks log-dir)
  (run-all-benchmarks-with-workers log-dir worker-counts))

(define (run-all-benchmarks-with-workers log-dir workers-list)
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
    (for ([workers workers-list])
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
     (string-replace template "@@TITLE@@" "Racket Benchmarks" #:all? #t)
     "@@DATA@@" json-data #:all? #t))

  ;; Write output
  (call-with-output-file output-file
    #:exists 'replace
    (λ (out) (write-string html out)))

  (printf "Report generated: ~a\n" output-file))

(module+ main
  (define log-dir "logs/racket")
  (define output-file "racket-results.html")
  (define custom-workers #f)

  (command-line
   #:program "run-racket-benchmarks"
   #:once-each
   [("--log-dir") dir "Directory for log files" (set! log-dir dir)]
   [("--output") file "Output HTML file" (set! output-file file)]
   [("--workers") counts "Comma-separated worker counts (e.g., 1,2,4,8)"
    (set! custom-workers (parse-worker-counts counts))])

  ;; Use custom workers if provided, otherwise default
  (define active-workers (or custom-workers worker-counts))

  (printf "Starting Racket benchmark suite...\n")
  (printf "Worker counts: ~a\n" active-workers)
  (printf "Log directory: ~a\n" log-dir)
  (printf "Output file: ~a\n\n" output-file)

  (run-all-benchmarks-with-workers log-dir active-workers)
  (generate-html-report log-dir output-file)

  (printf "\nDone! Open ~a in your browser to view results.\n" output-file))
