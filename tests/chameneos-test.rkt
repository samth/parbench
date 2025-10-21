#lang racket

(require rackunit
         "../benchmarks/shootout/chameneos.rkt")

(module+ test
  ;; Test basic execution - results will vary due to thread scheduling
  ;; but should always complete and return valid structure
  (define result (chameneos 40 #:colors '(blue red yellow)))
  (check-equal? (length result) 3)
  (check-true (andmap exact-nonnegative-integer? result))
  ;; Total meetings should equal 2*n (each meeting counted by both creatures)
  (check-equal? (second result) 80))
