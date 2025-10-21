#lang racket

(require rackunit
         "../benchmarks/racket/rows1b.rkt")

(module+ test
  (define rows 2000)
  (define seq (rows1b-sequential rows))
  (define par (rows1b-parallel rows #:workers 4 #:chunk-size 250))
  (check-true (rows1b-results=? seq par)))
