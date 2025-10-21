#lang racket

(require rackunit
         "../../benchmarks/mpl/samplesort.rkt")

;; Helper to check if vector is sorted
(define (sorted? vec)
  (for/and ([i (in-range (sub1 (vector-length vec)))])
    (<= (vector-ref vec i) (vector-ref vec (add1 i)))))

;; Test samplesort with known inputs
(check-equal? (samplesort-sequential #(3 1 4 1 5 9 2 6))
              #(1 1 2 3 4 5 6 9))
(check-equal? (samplesort-sequential #(5 4 3 2 1))
              #(1 2 3 4 5))
(check-equal? (samplesort-sequential #())
              #())
(check-equal? (samplesort-sequential #(42))
              #(42))

;; Test sequential is sorted
(define test-data (generate-random-vector 1000 42))
(define seq-sorted (samplesort-sequential test-data))
(check-true (sorted? seq-sorted) "Sequential result should be sorted")

;; Test parallel is sorted
(define par-sorted (samplesort-parallel test-data 4))
(check-true (sorted? par-sorted) "Parallel result should be sorted")

;; Test sequential and parallel produce same result
(check-equal? seq-sorted par-sorted
              "Sequential and parallel should produce same result")

(printf "samplesort tests passed\n")
