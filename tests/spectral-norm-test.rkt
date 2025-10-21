#lang racket

(require rackunit
         "../benchmarks/shootout/spectral-norm.rkt")

(module+ test
  (define n 32)
  (define seq (spectral-norm n #:workers 1 #:iterations 3))
  (define par (spectral-norm n #:workers 4 #:iterations 3))
  (check-true (< (abs (- seq par)) 1e-6)))
