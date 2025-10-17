#lang racket

(require rackunit
         "../../benchmarks/mpl/shuffle.rkt")

(module+ test
  ;; Test that shuffle produces valid permutation
  (define input (for/vector ([i (in-range 100)]) i))
  (define seq (shuffle-sequential input 42))
  (define par (shuffle-parallel input 3 42 20))

  ;; Both should be valid permutations
  (check-true (verify-permutation input seq))
  (check-true (verify-permutation input par))

  ;; Test with larger input
  (define large-input (for/vector ([i (in-range 10000)]) i))
  (define large-result (shuffle-parallel large-input 4 123 1000))
  (check-true (verify-permutation large-input large-result)))
