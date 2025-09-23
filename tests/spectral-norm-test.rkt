#lang racket

(require rackunit
         "../benchmarks/shootout/spectral-norm.rkt")

(module+ test
  (define n 60)
  (define seq (spectral-norm n #:workers 1 #:iterations 5))
  (define par (spectral-norm n #:workers 4 #:iterations 5))
  (check-true (< (abs (- seq par)) 1e-6)))
