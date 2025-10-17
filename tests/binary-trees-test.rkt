#lang racket

(require rackunit
         "../benchmarks/shootout/binary-trees.rkt")

(module+ test
  (define seq (binary-trees 6 #:workers 1))
  (define par (binary-trees 6 #:workers 4))
  (check-equal? seq par))
