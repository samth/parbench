#lang racket

(require rackunit
         "../../benchmarks/mpl/parens.rkt")

;; Test parentheses matching with known inputs
(define (test-parens str expected)
  (define data (list->vector (string->list str)))
  (define result (parens-sequential data))
  (check-equal? result expected
                (format "Parens '~a' should be ~a" str (if expected "balanced" "unbalanced"))))

;; Test cases
(test-parens "()" #t)
(test-parens "(())" #t)
(test-parens "()()" #t)
(test-parens "(()(()))" #t)
(test-parens "(" #f)
(test-parens ")" #f)
(test-parens "(()" #f)
(test-parens "())" #f)

;; Test sequential vs parallel equivalence
(define test-data (generate-parens 1000))
(define seq-result (parens-sequential test-data))
(define par-result (parens-parallel test-data 4))
(check-equal? seq-result par-result
              "Sequential and parallel parens should match")

