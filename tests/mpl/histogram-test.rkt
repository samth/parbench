#lang racket

(require rackunit
         "../../benchmarks/mpl/histogram.rkt")

(module+ test
  (define data (vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
  (define buckets 4)
  (define seq (histogram-sequential data buckets))
  (define par (histogram-parallel data buckets 3))
  (check-equal? par seq))
