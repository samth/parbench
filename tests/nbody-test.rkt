#lang racket

(require rackunit
         "../benchmarks/shootout/nbody.rkt")

(module+ test
  (define seq (nbody-simulation 50 #:workers 1))
  (define par (nbody-simulation 50 #:workers 4))
  (define tolerance 1e-6)
  (check-true (< (abs (- (first seq) (first par))) tolerance))
  (check-true (< (abs (- (second seq) (second par))) tolerance)))
