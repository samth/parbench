#lang racket

(require rackunit
         "../benchmarks/shootout/fasta.rkt")

(module+ test
  (define expected '(100 8609 2612064341))
  (check-equal? (fasta 2 #:workers 1) expected)
  (check-equal? (fasta 2 #:workers 4) expected))
