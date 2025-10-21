#lang racket

(require rackunit
         "../../benchmarks/mpl/merge-sort.rkt")

;; Helper to check if vector is sorted
(define (sorted? vec)
  (for/and ([i (in-range (sub1 (vector-length vec)))])
    (<= (vector-ref vec i) (vector-ref vec (add1 i)))))

;; Test merge sort with known inputs
(define (test-sort input expected)
  (define result (merge-sort-sequential input))
  (check-equal? result expected
                (format "Merge sort should produce correct result")))

;; Test cases
(test-sort #(3 1 4 1 5 9 2 6) #(1 1 2 3 4 5 6 9))
(test-sort #(5 4 3 2 1) #(1 2 3 4 5))
(test-sort #(1 2 3 4 5) #(1 2 3 4 5))  ; Already sorted
(test-sort #() #())  ; Empty
(test-sort #(42) #(42))  ; Single element
(test-sort #(2 1) #(1 2))  ; Two elements

;; Test with duplicates
(test-sort #(3 3 3 1 1 2 2) #(1 1 2 2 3 3 3))

;; Test sequential is sorted
(define test-data (generate-random-vector 1000 42))
(define seq-sorted (merge-sort-sequential test-data))
(check-true (sorted? seq-sorted) "Sequential result should be sorted")

;; Test parallel is sorted
(define par-sorted (merge-sort-parallel test-data 4))
(check-true (sorted? par-sorted) "Parallel result should be sorted")

;; Test sequential and parallel produce same result
(check-equal? seq-sorted par-sorted
              "Sequential and parallel should produce same result")

;; Test with larger data
(define large-data (generate-random-vector 10000 123))
(define large-seq (merge-sort-sequential large-data))
(define large-par (merge-sort-parallel large-data 4))
(check-true (sorted? large-seq) "Large sequential should be sorted")
(check-true (sorted? large-par) "Large parallel should be sorted")
(check-equal? large-seq large-par "Large sequential and parallel should match")

(printf "merge-sort tests passed\n")
