#lang racket

(require rackunit
         racket/set
         "../../benchmarks/mpl/mis.rkt")

(define cycle4
  (vector
   (vector 1 3)
   (vector 0 2)
   (vector 1 3)
   (vector 0 2)))

(module+ test
  (define seq (mis-sequential cycle4))
  (define par (mis-parallel cycle4 3 123))
  (check-true (verify-mis cycle4 seq))
  (check-true (verify-mis cycle4 par))
  (check-equal? (length par) (length seq)))
