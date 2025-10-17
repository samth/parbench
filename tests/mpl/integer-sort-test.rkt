#lang racket

(require rackunit
         "../../benchmarks/mpl/integer-sort.rkt")

(module+ test
  (define base (vector 3 0 2 1 3 2 0 1))
  (define seq-input (vector-copy base))
  (define par-input (vector-copy base))

  (define seq (integer-sort-sequential seq-input 4))
  (define par (integer-sort-parallel par-input 4 3))

  (check-equal? seq #(0 0 1 1 2 2 3 3))
  (check-equal? par seq)

  (check-equal? seq-input base)
  (check-equal? par-input base)

  (check-equal? (integer-sort-parallel (vector-copy base) 4 1) seq))
