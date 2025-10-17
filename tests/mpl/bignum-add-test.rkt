#lang racket

(require rackunit
         "../../benchmarks/mpl/bignum-add.rkt")

(module+ test
  ;; Test simple addition
  (define a (vector 1 2 3 4))
  (define b (vector 5 6 7 8))
  (define-values (seq-result seq-carry) (bignum-add-sequential a b))
  (define-values (par-result par-carry) (bignum-add-parallel a b 2 2))

  (check-equal? par-carry seq-carry)
  (check-equal? (vector-length par-result) (vector-length seq-result))

  ;; Test with larger numbers
  (define large-a (make-random-bignum 100 42))
  (define large-b (make-random-bignum 100 43))
  (define-values (seq-large seq-carry-large) (bignum-add-sequential large-a large-b))
  (define-values (par-large par-carry-large) (bignum-add-parallel large-a large-b 3 20))

  (check-equal? par-carry-large seq-carry-large)
  (check-equal? par-large seq-large))
