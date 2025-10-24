#lang racket

(require racket/cmdline
         racket/path
         racket/file
         racket/system
         racket/list
         racket/string
         racket/runtime-path
         json
         "analysis.rkt")

(provide run-and-visualize)

(define-runtime-path visualization-template-path
  "templates/visualization.html")

;; Generate JSON data from benchmark summaries
(define (summaries->json summaries)
  (define grouped (group-by summary-name summaries))
  (hasheq
   'benchmarks
   (for/list ([group grouped])
     (define bench-name (summary-name (first group)))
     (hasheq
      'name (symbol->string bench-name)
      'variants
      (for/list ([s group])
        (hasheq
         'variant (symbol->string (summary-variant s))
         'worker (summary-worker s)
         'count (summary-count s)
         'real_mean (summary-real-mean s)
         'real_stddev (summary-real-stddev s)
         'real_min (summary-real-min s)
         'real_max (summary-real-max s)
         'cpu_mean (summary-cpu-mean s)
         'gc_mean (summary-gc-mean s)))))))

(define (generate-html json-data title)
  (define template (file->string visualization-template-path))
  (define with-title (string-replace template "@@TITLE@@" title #:all? #t))
  (string-replace with-title "@@DATA@@" json-data #:all? #t))

;; Main visualization function
(define (run-and-visualize #:suite-names [suite-names '("all")]
                            #:config-path [config-path #f]
                            #:log-dir [log-dir "logs"]
                            #:output [output "benchmark-results.html"]
                            #:title [title "Benchmark Results"]
                            #:run-benchmarks? [run-benchmarks? #t])

  (when run-benchmarks?
    (printf "Running benchmarks...\n")
    (define run-suite-path (build-path (current-directory) "benchmarks" "run-suite.rkt"))
    (define args (append (list (path->string run-suite-path))
                        (apply append (for/list ([suite suite-names])
                                       (list "--suite" suite)))
                        (list "--log-dir" log-dir)
                        (if config-path
                            (list "--config" config-path)
                            '())))

    (define-values (proc out in err)
      (apply subprocess #f #f #f (find-executable-path "racket") args))
    (subprocess-wait proc)
    (close-output-port in)
    (close-input-port out)
    (close-input-port err)

    (define exit-code (subprocess-status proc))
    (unless (zero? exit-code)
      (error 'run-and-visualize "Benchmark suite failed with exit code: ~a" exit-code)))

  (printf "\nGenerating visualization...\n")

  ;; Collect all .sexp files from log directory
  (define log-files
    (if (directory-exists? log-dir)
        (for/list ([file (directory-list log-dir)]
                   #:when (regexp-match? #rx"\\.sexp$" (path->string file)))
          (build-path log-dir file))
        '()))

  (when (null? log-files)
    (error 'run-and-visualize "No .sexp log files found in ~a" log-dir))

  (printf "Found ~a log file(s)\n" (length log-files))

  ;; Load and aggregate results
  (define summaries (load-summaries (map path->string log-files)))

  (when (null? summaries)
    (error 'run-and-visualize "No benchmark data found in log files"))

  (printf "Loaded ~a benchmark result(s)\n" (length summaries))

  ;; Generate JSON
  (define json-hash (summaries->json summaries))
  (define json-string (jsexpr->string json-hash))

  ;; Generate HTML
  (define html (generate-html json-string title))

  ;; Write output file
  (call-with-output-file output
    #:exists 'replace
    (λ (out) (display html out)))

  (printf "✓ Visualization saved to: ~a\n" output)
  (printf "\nOpen the file in a web browser to view the interactive dashboard.\n"))

(module+ main
  (define suite-names '())
  (define config-path #f)
  (define log-dir "logs")
  (define output "benchmark-results.html")
  (define title "Benchmark Results")
  (define run-benchmarks? #t)

  (void
   (command-line
    #:program "visualize.rkt"
    #:once-each
    [("--suite" "-s") suite "Suite to run: racket, shootout, nas, mpl, or all"
     (set! suite-names (cons suite suite-names))]
    [("--config" "-c") path "Configuration file (S-expression)"
     (set! config-path path)]
    [("--log-dir" "-l") dir "Directory for log files (default: logs)"
     (set! log-dir dir)]
    [("--output" "-o") file "Output HTML file (default: benchmark-results.html)"
     (set! output file)]
    [("--title" "-t") t "Dashboard title (default: Benchmark Results)"
     (set! title t)]
    [("--no-run") "Skip running benchmarks, only generate visualization from existing logs"
     (set! run-benchmarks? #f)]
    #:args ()
    (void)))

  (when (null? suite-names)
    (set! suite-names '("all")))

  (run-and-visualize #:suite-names (reverse suite-names)
                     #:config-path config-path
                     #:log-dir log-dir
                     #:output output
                     #:title title
                     #:run-benchmarks? run-benchmarks?))
