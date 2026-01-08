#lang racket

(require rackunit
         "../../benchmarks/mpl/bignum-add.rkt")

(module+ test
  ;; Test simple addition with bytes
  (define a #"\x01\x02\x03\x04")
  (define b #"\x05\x06\x07\x08")
  (define seq-result (bignum-add-sequential a b))
  (define par-result (bignum-add-parallel a b 2))

  (check-equal? par-result seq-result)

  ;; Test with larger numbers
  (define large-a (make-random-bignum 100 42))
  (define large-b (make-random-bignum 100 43))
  (define seq-large (bignum-add-sequential large-a large-b))
  (define par-large (bignum-add-parallel large-a large-b 3))

  (check-equal? par-large seq-large))
