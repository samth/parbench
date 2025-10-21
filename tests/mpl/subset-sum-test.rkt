#lang racket

(require rackunit
         "../../benchmarks/mpl/subset-sum.rkt")

;; Test subset sum with known inputs
(define (test-subset-sum bag goal expected)
  (define result (subset-sum-sequential bag goal))
  (check-equal? result expected
                (format "Subset sum for goal ~a should be ~a" goal expected)))

;; Test cases
(test-subset-sum #(3 34 4 12 5 2) 9 #t)   ; 4 + 5 = 9
(test-subset-sum #(3 34 4 12 5 2) 30 #f)  ; No subset sums to 30
(test-subset-sum #(1 2 3 4 5) 10 #t)      ; 1+2+3+4 = 10
(test-subset-sum #(10 20 30) 25 #f)       ; No subset sums to 25

(printf "subset-sum tests passed\n")
