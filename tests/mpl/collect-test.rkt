#lang racket

(require rackunit
         "../../benchmarks/mpl/collect.rkt")

(module+ test
  ;; Test simple collect
  (define input (vector 1 2 3 4 5 6 7 8 9 10))
  (define even-result (collect-sequential input even?))
  (check-equal? even-result (vector 2 4 6 8 10))

  ;; Test parallel version matches sequential
  (define par-even (collect-parallel input even? 2))
  (check-equal? par-even even-result)

  ;; Test with generated data
  (define large-input (generate-input-vector 10000 42))
  (define large-seq (collect-sequential large-input even?))
  (define large-par (collect-parallel large-input even? 3))
  (check-equal? large-par large-seq))
