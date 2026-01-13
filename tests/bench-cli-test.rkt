#lang at-exp racket
;; Expect test for ./bench CLI output
;; Uses recspecs to verify the CLI produces expected output format
;;
;; NOTE: When using RECSPECS_UPDATE=1 with multiple @expect forms in the same
;; file, there is a bug where recspecs can corrupt the file by miscalculating
;; position/span when updating multiple expectations. To update these tests
;; safely, update them ONE AT A TIME using RECSPECS_UPDATE_TEST="test name".
;; See: https://github.com/samth/recspecs/issues (if reported)

(require rackunit
         rackunit/text-ui
         recspecs)

;; Filter out timing values and trailing whitespace for stable comparison
(define (normalize-bench-output s)
  ;; Replace timing numbers (0, 0.0, 123, 456.7, etc.) with N
  (define with-n (regexp-replace* #px"\\b[0-9]+\\.?[0-9]*\\b" s "N"))
  ;; Trim each line's trailing whitespace
  (string-join
   (for/list ([line (string-split with-n "\n")])
     (string-trim line #:left? #f))
   "\n"))

(define bench-cli-tests
  (test-suite "bench CLI tests"

    (test-case "bench --list shows benchmarks"
      @expect[(system "cd /home/samth/work/parbench && ./bench --list 2>&1" #:set-pwd? #f)]{Available benchmarks:

MPL (27):
  histogram, integer-sort, bfs, convex-hull, mis, msf, suffix-array, primes, merge-sort, samplesort, tokens, nqueens, dedup, word-count, fib, shuffle, grep, palindrome, parens, mcss, flatten, collect, bignum-add, subset-sum, triangle-count, connectivity, centrality

Shootout (6):
  binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna

Racket (3):
  bmbench, richards, rows1b
})

    (test-case "bench runs fib with small work"
      (parameterize ([recspecs-output-filter normalize-bench-output])
        @expect[(system "cd /home/samth/work/parbench && ./bench --work 0.001 --iterations 1 --cores 1 fib 2>&1" #:set-pwd? #f)]{
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


}))))

(module+ test
  (run-tests bench-cli-tests))
