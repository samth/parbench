#lang at-exp racket
;; Expect test for ./bench CLI output
;; Uses recspecs to verify the CLI produces expected output format

(require racket/runtime-path
         racket/port
         recspecs)

;; Get the project root directory (parent of tests/)
(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))
(define bench-script (build-path project-root "bench"))

;; Run bench command and display output (for recspecs to capture)
(define (run-bench . args)
  (parameterize ([current-directory project-root])
    (define racket-exe (find-system-path 'exec-file))
    ;; Use subprocess for explicit output capture
    (define-values (proc stdout stdin stderr)
      (apply subprocess #f #f #f racket-exe (path->string bench-script) args))
    (close-output-port stdin)
    (define output (port->string stdout))
    (close-input-port stdout)
    (close-input-port stderr)
    (subprocess-wait proc)
    (display output)))

;; Filter to normalize timing values for stable comparison
(define (normalize-bench-output s)
  ;; Replace timing numbers with N
  (define with-n (regexp-replace* #px"\\b[0-9]+\\.?[0-9]*\\b" s "N"))
  ;; Trim each line's trailing whitespace
  (string-join
   (for/list ([line (string-split with-n "\n")])
     (string-trim line #:left? #f))
   "\n"))

(module+ test
  @expect[(run-bench "--list")]{Available benchmarks:

MPL (27):
  histogram, integer-sort, bfs, convex-hull, mis, msf, suffix-array, primes, merge-sort, samplesort, tokens, nqueens, dedup, word-count, fib, shuffle, grep, palindrome, parens, mcss, flatten, collect, bignum-add, subset-sum, triangle-count, connectivity, centrality

Shootout (6):
  binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna

Racket (5):
  bmbench, richards, rows1b, inverted-index, inverted-index-opt
}

  (parameterize ([recspecs-output-filter normalize-bench-output])
    @expect[(run-bench "--work" "0.001" "--iterations" "1" "--cores" "1" "fib")]{
Parbench

Running mpl benchmarks...
  fib

========================================
  Results Summary
========================================

                              seq                  N workers
Benchmark               mean/median/min         mean/median/min
--------------------------------------------------------------------
fib                        N/N/N               N/N/N
}))
