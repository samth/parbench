#lang racket

(require rackunit
         "../../benchmarks/mpl/mcss.rkt")

;; Test MCSS with known inputs
(define (test-mcss data expected)
  (define result (mcss-sequential data))
  (check-= result expected 0.001
           (format "MCSS should be ~a" expected)))

;; Test cases
(test-mcss #(-2.0 1.0 -3.0 4.0 -1.0 2.0 1.0 -5.0 4.0) 6.0)  ; [4,-1,2,1]
(test-mcss #(1.0 2.0 3.0) 6.0)  ; All positive
(test-mcss #(-1.0 -2.0 -3.0) -1.0)  ; All negative (best is single element)

;; Test sequential vs parallel equivalence
(define test-data (generate-random-data 1000 42))
(define seq-result (mcss-sequential test-data))
(define par-result (mcss-parallel test-data 4))
(check-= seq-result par-result 0.001
         "Sequential and parallel MCSS should match")

(printf "mcss tests passed\n")
