#lang racket

(require racket/file
         racket/path
         json
         "benchmarks/tools/analysis.rkt")

(define (generate-html-report log-dir output-file)
  (printf "Generating HTML report...\n")

  ;; Read all log files
  (define log-files
    (for/list ([f (in-directory log-dir)]
               #:when (equal? (path-get-extension f) #".sexp"))
      f))

  (define summaries (load-summaries (map path->string log-files)))
  (define raw-data-list (load-raw-data (map path->string log-files)))

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
           ;; Find corresponding raw data
           (define raw-entry
             (findf (λ (rd)
                      (and (eq? (raw-data-name rd) (summary-name s))
                           (eq? (raw-data-variant rd) (summary-variant s))
                           (equal? (raw-data-worker rd) (summary-worker s))))
                    raw-data-list))
           (hasheq
            'variant (symbol->string (summary-variant s))
            'worker (summary-worker s)
            'count (summary-count s)
            'real_mean (summary-real-mean s)
            'real_stddev (summary-real-stddev s)
            'real_min (summary-real-min s)
            'real_max (summary-real-max s)
            'real_values (if raw-entry (raw-data-real-values raw-entry) '())
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

(generate-html-report "logs/shootout" "shootout-results.html")
