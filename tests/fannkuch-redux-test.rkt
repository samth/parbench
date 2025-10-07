#lang racket

(require rackunit
         "../benchmarks/shootout/fannkuch-redux.rkt")

(module+ test
  (define-values (seq-flips seq-checksum) (fannkuch-redux 8 #:workers 1))
  (define-values (par-flips par-checksum) (fannkuch-redux 8 #:workers 4))
  (check-equal? seq-flips par-flips)
  (check-equal? seq-checksum par-checksum))
