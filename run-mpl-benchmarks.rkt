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
;; Tuned so sequential version takes ~500-600ms per iteration
(define benchmark-configs
  '(;; 4 benchmarks with CAS-based parallel primitives, tuned for ~500ms sequential
    (histogram "--n" "200000000" "--repeat" "10")    ; 515ms sequential
    (integer-sort "--n" "50000000" "--range" "10000" "--repeat" "10")  ; 475ms seq, shows speedup with small range
    (bfs "--n" "8000000" "--graph-type" "grid" "--repeat" "10")  ; 532ms sequential (grid graph avoids O(n²) gen)
    (convex-hull "--n" "2500000" "--repeat" "10")    ; 566ms sequential
    ;; 5 additional benchmarks with thread pool parallelism
    (mis "--n" "1500000" "--repeat" "10")            ; 538ms sequential
    (msf "--n" "120000" "--repeat" "10")             ; 546ms sequential
    (suffix-array "--n" "600000" "--repeat" "10")    ; 772ms sequential
    (primes "--n" "50000000" "--repeat" "10")        ; 214ms sequential
    (merge-sort "--n" "5000000" "--repeat" "10")     ; 957ms sequential
    ;; 5 more benchmarks with thread pool parallelism
    (samplesort "--n" "2000000" "--repeat" "10")     ; 643ms sequential
    (tokens "--size" "5000000" "--repeat" "10")      ; 748ms sequential
    (nqueens "--n" "13" "--repeat" "10")             ; 1635ms sequential (excellent speedup)
    (dedup "--n" "5000000" "--unique" "500000" "--repeat" "10")  ; 603ms sequential
    (word-count "--size" "40000000" "--repeat" "10") ; 543ms sequential
    ;; 5 more benchmarks with thread pool parallelism
    (fib "--n" "42" "--threshold" "30" "--repeat" "10")  ; 771ms sequential (excellent speedup)
    (shuffle "--n" "5000000" "--chunk-size" "500000" "--repeat" "10")  ; 564ms seq (parallel slower)
    (grep "--lines" "2000000" "--repeat" "10")       ; 611ms sequential
    (palindrome "--n" "500000000" "--repeat" "10")   ; 456ms sequential
    (parens "--n" "150000000" "--repeat" "10")       ; 539ms sequential
    ;; 5 more benchmarks (subset-sum shows excellent speedup, others memory-bound)
    (mcss "--n" "100000000" "--repeat" "10")         ; 395ms seq (parallel slower - tuple alloc overhead)
    (flatten "--n" "600000" "--avg-size" "100" "--repeat" "10")  ; 700ms seq (memory-bound)
    (collect "--n" "15000000" "--repeat" "10")       ; 385ms seq (memory-bound)
    (bignum-add "--n" "50000000" "--chunk-size" "5000000" "--repeat" "10")  ; 650ms seq (parallel slower - sequential carry)
    (subset-sum "--n" "28" "--goal" "10000" "--max-value" "100" "--repeat" "10")  ; 600ms seq (5.8x speedup!)
    ;; 3 final graph benchmarks with thread pool parallelism
    (triangle-count "--n" "17000" "--edges" "1000000" "--repeat" "10")  ; 460ms seq (3.9x speedup)
    (connectivity "--n" "2000000" "--edges" "8000000" "--repeat" "10")  ; 720ms seq (4.5x speedup)
    (centrality "--n" "20000" "--edges" "100000" "--repeat" "10")))     ; 497ms seq (no speedup - BFS dominates)

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

;; Parse comma-separated worker counts string into list of numbers
(define (parse-worker-counts str)
  (map string->number (string-split str ",")))

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
  (define custom-workers #f)

  (command-line
   #:program "run-mpl-benchmarks"
   #:once-each
   [("--log-dir") dir "Directory for log files" (set! log-dir dir)]
   [("--output") file "Output HTML file" (set! output-file file)]
   [("--workers") counts "Comma-separated worker counts (e.g., 1,2,4,8)"
    (set! custom-workers (parse-worker-counts counts))])

  ;; Use custom workers if provided, otherwise default
  (define active-workers (or custom-workers worker-counts))

  (printf "Starting MPL benchmark suite...\n")
  (printf "Worker counts: ~a\n" active-workers)
  (printf "Log directory: ~a\n" log-dir)
  (printf "Output file: ~a\n\n" output-file)

  (run-all-benchmarks-with-workers log-dir active-workers)
  (generate-html-report log-dir output-file)

  (printf "\nDone! Open ~a in your browser to view results.\n" output-file))
