#lang racket

(require racket/cmdline
         racket/file
         racket/path
         racket/system
         racket/list
         json
         "benchmarks/tools/analysis.rkt")

;; Benchmark configurations: (name args...)
;; Tuned so sequential version takes ~500-700ms per iteration
(define benchmark-configs
  '(;; 5 benchmarks tuned for sequential time >0.5s
    (histogram "--n" "200000000" "--repeat" "10")    ; ~600ms
    (integer-sort "--n" "25000000" "--repeat" "10")  ; ~588ms
    (bfs "--n" "75000" "--repeat" "10")              ; ~508ms
    (convex-hull "--n" "1500000" "--repeat" "10")    ; ~450ms (n=2M had slow generation)
    (mcss "--n" "150000000" "--repeat" "10")))       ; ~684ms (replaced mis - algorithm too fast)

;; Full list of all 27 benchmarks (uncomment when ready)
#;(define benchmark-configs-full
  '(;; Simple array operations - large arrays for measurable time
    (histogram "--n" "10000000" "--repeat" "10")
    (integer-sort "--n" "10000000" "--repeat" "10")
    (mcss "--n" "10000000" "--repeat" "10")
    (merge-sort "--n" "10000000" "--repeat" "10")
    (samplesort "--n" "10000000" "--repeat" "10")
    (shuffle "--n" "10000000" "--repeat" "10")
    (tokens "--n" "10000000" "--repeat" "10")
    (word-count "--n" "10000000" "--repeat" "10")
    (dedup "--n" "10000000" "--repeat" "10")
    (collect "--n" "10000000" "--repeat" "10")

    ;; Graph algorithms - smaller sizes due to O(n^2) graph generation
    (bfs "--n" "50000" "--repeat" "10")
    (convex-hull "--n" "1000000" "--repeat" "10")
    (mis "--n" "100000" "--repeat" "10")
    (msf "--n" "10000" "--repeat" "10")
    (connectivity "--n" "50000" "--repeat" "10")
    (centrality "--n" "10000" "--repeat" "10")
    (triangle-count "--n" "10000" "--repeat" "10")

    ;; String/text operations
    (suffix-array "--n" "100000" "--repeat" "10")
    (grep "--num-lines" "1000000" "--repeat" "10")
    (palindrome "--n" "100000000" "--repeat" "10")
    (parens "--n" "100000000" "--repeat" "10")
    (primes "--n" "100000000" "--repeat" "10")

    ;; Tree/recursive operations
    (flatten "--n" "50000" "--repeat" "10")
    (fib "--n" "45" "--repeat" "10")
    (nqueens "--n" "14" "--repeat" "10")

    ;; Numeric/math operations
    (bignum-add "--n" "1000000" "--repeat" "10")
    (subset-sum "--n" "30" "--repeat" "10")))

(define worker-counts '(1 2 4 6 8))

(define (run-benchmark-sequential name args log-path)
  (define benchmark-path
    (build-path "benchmarks" "mpl" (format "~a.rkt" name)))
  (define cmd-args
    (append args (list "--workers" "1" "--log" log-path)))
  (printf "Running ~a sequential (workers=1)...\n" name)
  (define result (apply system* (find-executable-path "racket") benchmark-path cmd-args))
  (unless result
    (eprintf "Warning: ~a sequential failed\n" name)))

(define (run-benchmark-parallel name args worker-count log-path)
  (define benchmark-path
    (build-path "benchmarks" "mpl" (format "~a.rkt" name)))
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

  (define file-paths (map path->string log-files))
  (define summaries (load-summaries file-paths))
  (define raw-data-list (load-raw-data file-paths))

  ;; Build a lookup table for raw values: (name variant worker) -> real-values
  (define raw-lookup
    (for/hash ([rd raw-data-list])
      (values (list (raw-data-name rd) (raw-data-variant rd) (raw-data-worker rd))
              (raw-data-real-values rd))))

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
           (define key (list (summary-name s) (summary-variant s) (summary-worker s)))
           (define raw-values (hash-ref raw-lookup key '()))
           (hasheq
            'variant (symbol->string (summary-variant s))
            'worker (summary-worker s)
            'count (summary-count s)
            'real_mean (summary-real-mean s)
            'real_stddev (summary-real-stddev s)
            'real_min (summary-real-min s)
            'real_max (summary-real-max s)
            'cpu_mean (summary-cpu-mean s)
            'gc_mean (summary-gc-mean s)
            'real_values raw-values)))))))

  ;; Read HTML template
  (define template-path
    (build-path "benchmarks" "tools" "templates" "visualization.html"))
  (define template (file->string template-path))

  ;; Replace placeholders
  (define html
    (string-replace
     (string-replace template "@@TITLE@@" "MPL Benchmarks" #:all? #t)
     "@@DATA@@" json-data #:all? #t))

  ;; Write output
  (call-with-output-file output-file
    #:exists 'replace
    (lambda (out) (write-string html out)))

  (printf "Report generated: ~a\n" output-file))

(module+ main
  (define log-dir "logs/mpl")
  (define output-file "mpl-results.html")

  (command-line
   #:program "run-mpl-benchmarks"
   #:once-each
   [("--log-dir") dir "Directory for log files" (set! log-dir dir)]
   [("--output") file "Output HTML file" (set! output-file file)])

  (printf "Starting MPL benchmark suite...\n")
  (printf "Worker counts: ~a\n" worker-counts)
  (printf "Log directory: ~a\n" log-dir)
  (printf "Output file: ~a\n\n" output-file)

  (run-all-benchmarks log-dir)
  (generate-html-report log-dir output-file)

  (printf "\nDone! Open ~a in your browser to view results.\n" output-file))
