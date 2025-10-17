#lang racket

(require rackunit
         "../../benchmarks/mpl/flatten.rkt")

(module+ test
  ;; Test simple flatten
  (define nested (vector (vector 1 2) (vector 3 4 5) (vector 6)))
  (define expected (vector 1 2 3 4 5 6))
  (define seq (flatten-sequential nested))
  (check-equal? seq expected)

  ;; Test parallel version matches sequential
  (define par (flatten-parallel nested 2))
  (check-equal? par seq)

  ;; Test with generated data
  (define large-nested (generate-nested-vector 100 50 42))
  (define large-seq (flatten-sequential large-nested))
  (define large-par (flatten-parallel large-nested 3))
  (check-equal? large-par large-seq))
