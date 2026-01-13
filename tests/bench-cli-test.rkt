#lang at-exp racket
;; Expect test for ./bench CLI output

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         recspecs)

;; Get the project root directory (parent of tests/)
(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))
(define bench-script (build-path project-root "bench"))

;; Filter out timing values and trailing whitespace for stable comparison
(define (normalize-bench-output s)
  ;; Replace timing numbers (0, 0.0, 123, 456.7, etc.) with N
  (define with-n (regexp-replace* #px"\\b[0-9]+\\.?[0-9]*\\b" s "N"))
  ;; Trim each line's trailing whitespace
  (string-join
   (for/list ([line (string-split with-n "\n")])
     (string-trim line #:left? #f))
   "\n"))

;; Run bench command and capture stdout only (not stderr)
;; Uses find-system-path to get the currently running racket executable,
;; which is more reliable than depending on racket being in PATH.
(define (run-bench . args)
  (parameterize ([current-directory project-root])
    ;; Use the same racket that's running these tests - this works even when
    ;; racket is not in PATH (e.g., in package build environments)
    (define racket-exe (find-system-path 'exec-file))
    (with-output-to-string
      (lambda ()
        (apply system* racket-exe (path->string bench-script) args)))))

(define bench-cli-tests
  (test-suite "bench CLI tests"

    (test-case "bench --list shows benchmarks"
      (check-equal?
       (string-trim (run-bench "--list"))
       @string-append{Available benchmarks:

MPL (27):
  histogram, integer-sort, bfs, convex-hull, mis, msf, suffix-array, primes, merge-sort, samplesort, tokens, nqueens, dedup, word-count, fib, shuffle, grep, palindrome, parens, mcss, flatten, collect, bignum-add, subset-sum, triangle-count, connectivity, centrality

Shootout (6):
  binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna

Racket (3):
  bmbench, richards, rows1b}))

    (test-case "bench runs fib with small work"
      (define output (normalize-bench-output (run-bench "--work" "0.001" "--iterations" "1" "--cores" "1" "fib")))
      ;; Check that output contains expected sections
      (check-true (string-contains? output "Parbench") "Should show Parbench header")
      (check-true (string-contains? output "Running mpl benchmarks") "Should show running message")
      (check-true (string-contains? output "fib") "Should mention fib benchmark")
      (check-true (string-contains? output "Results Summary") "Should show results summary")
      (check-true (string-contains? output "Benchmark") "Should have benchmark column header"))))

(module+ test
  (run-tests bench-cli-tests))
