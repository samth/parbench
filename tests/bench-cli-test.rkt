#lang at-exp racket
;; Expect test for ./bench CLI output
;; Uses recspecs/shell for subprocess handling

(require racket/runtime-path
         recspecs
         recspecs/shell)

;; Get the project root directory (parent of tests/)
(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))
(define bench-script (path->string (build-path project-root "bench")))

;; Build command with racket executable for ./bench
(define (bench-cmd . args)
  (string-join (cons (path->string (find-system-path 'exec-file))
                     (cons bench-script args))
               " "))

;; Build command for raco parbench
(define (raco-parbench-cmd . args)
  (string-join (list* (path->string (find-system-path 'exec-file))
                      "-l-" "raco" "parbench" args)
               " "))

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
  (parameterize ([current-directory project-root])
    @expect/shell[(bench-cmd "--list")]{Available benchmarks:

MPL (27):
  histogram, integer-sort, bfs, convex-hull, mis, msf, suffix-array, primes, merge-sort, samplesort, tokens, nqueens, dedup, word-count, fib, shuffle, grep, palindrome, parens, mcss, flatten, collect, bignum-add, subset-sum, triangle-count, connectivity, centrality

Shootout (6):
  binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna

Racket (5):
  bmbench, richards, rows1b, inverted-index, inverted-index-opt
}

    (parameterize ([recspecs-output-filter normalize-bench-output])
      @expect/shell[(bench-cmd "--work" "0.001" "--iterations" "1" "--cores" "1" "fib")]{
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
})

    ;; Test raco parbench wrapper (important for package installations)
    @expect/shell[(raco-parbench-cmd "--list")]{Available benchmarks:

MPL (27):
  histogram, integer-sort, bfs, convex-hull, mis, msf, suffix-array, primes, merge-sort, samplesort, tokens, nqueens, dedup, word-count, fib, shuffle, grep, palindrome, parens, mcss, flatten, collect, bignum-add, subset-sum, triangle-count, connectivity, centrality

Shootout (6):
  binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna

Racket (5):
  bmbench, richards, rows1b, inverted-index, inverted-index-opt
}))
