#lang racket

(require rackunit
         "../benchmarks/shootout/mandelbrot.rkt")

(module+ test
  (define seq (mandelbrot 100 #:workers 1))
  (define par (mandelbrot 100 #:workers 4))
  (check-equal? seq par))
