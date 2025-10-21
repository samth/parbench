#lang racket

(require rackunit
         "../../benchmarks/mpl/primes.rkt")

;; Test primes with known counts
;; https://oeis.org/A006880
(define known-prime-counts
  '((10 . 4)
    (100 . 25)
    (1000 . 168)
    (10000 . 1229)
    (100000 . 9592)
    (1000000 . 78498)))

(for ([pair (in-list known-prime-counts)])
  (define n (car pair))
  (define expected-count (cdr pair))
  (define actual-count (primes-sequential n))
  (check-equal? actual-count expected-count
                (format "Prime count up to ~a should be ~a" n expected-count)))

;; Test sequential vs parallel equivalence
(define test-n 100000)
(define seq-count (primes-sequential test-n))
(define par-count (primes-parallel test-n 4))
(check-equal? seq-count par-count
              (format "Sequential and parallel should match for n=~a" test-n))

(printf "primes tests passed\n")
