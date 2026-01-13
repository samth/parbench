#lang racket

(require rackunit
         "../../benchmarks/mpl/dedup.rkt")

;; Test deduplication with known inputs
(define (test-dedup input expected-size)
  (define result (dedup-sequential input))
  (check-equal? (vector-length result) expected-size
                (format "Dedup should produce ~a unique elements" expected-size)))

;; Test cases
(test-dedup #(1 2 3 4 5) 5)           ; No duplicates
(test-dedup #(1 1 2 2 3 3) 3)         ; All duplicates
(test-dedup #(1 2 1 3 2 4) 4)         ; Mixed
(test-dedup #(5 5 5 5 5) 1)           ; All same

;; Test that deduplication preserves uniqueness
(define data #(1 2 3 2 4 3 5))
(define deduped (dedup-sequential data))
(define unique-set (list->set (vector->list deduped)))
(check-equal? (set-count unique-set) (vector-length deduped)
              "All elements in deduped result should be unique")

;; Test sequential vs parallel equivalence
(define test-data (generate-data-with-duplicates 1000 100 42))
(define seq-result (dedup-sequential test-data))
(define par-result (dedup-parallel test-data 4))
;; Both should have same number of unique elements
(check-equal? (vector-length seq-result) (vector-length par-result)
              "Sequential and parallel dedup should produce same size")

