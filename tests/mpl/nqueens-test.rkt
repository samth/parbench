#lang racket

(require rackunit
         "../../benchmarks/mpl/nqueens.rkt")

;; Test N-Queens with known solutions
(define (test-nqueens n expected-solutions)
  (define result (nqueens-sequential n))
  (check-equal? result expected-solutions
                (format "N-Queens with n=~a should have ~a solutions" n expected-solutions)))

;; Known N-Queens solution counts
(test-nqueens 1 1)
(test-nqueens 4 2)
(test-nqueens 8 92)

