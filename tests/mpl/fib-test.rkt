#lang racket

(require rackunit
         "../../benchmarks/mpl/fib.rkt")

(module+ test
  ;; Test small Fibonacci numbers
  (check-equal? (fib-sequential 0) 0)
  (check-equal? (fib-sequential 1) 1)
  (check-equal? (fib-sequential 5) 5)
  (check-equal? (fib-sequential 10) 55)
  (check-equal? (fib-sequential 15) 610)

  ;; Test parallel version matches sequential
  (define n 20)
  (define seq (fib-sequential n))
  (define par (fib-parallel n 3))
  (check-equal? par seq))
