#lang racket

(require rackunit
         "../../benchmarks/mpl/suffix-array.rkt")

(module+ test
  (define text "banana")
  (define seq (suffix-array-sequential text))
  (define par (suffix-array-parallel text 3))
  (check-true (verify-suffix-array text seq))
  (check-true (verify-suffix-array text par))
  (check-equal? par seq))
