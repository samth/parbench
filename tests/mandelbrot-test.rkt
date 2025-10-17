#lang racket

(require rackunit
         "../benchmarks/shootout/mandelbrot.rkt")

(module+ test
  (define seq (mandelbrot 60 #:workers 1))
  (define par (mandelbrot 60 #:workers 4))
  (check-equal? seq par))
