#lang racket

(require rackunit
         "../../benchmarks/mpl/word-count.rkt")

;; Test word count with known inputs
(define (test-wc text expected-lines expected-words expected-bytes)
  (match-define (list lines words bytes) (word-count-sequential text))
  (check-equal? lines expected-lines
                (format "Lines should be ~a" expected-lines))
  (check-equal? words expected-words
                (format "Words should be ~a" expected-words))
  (check-equal? bytes expected-bytes
                (format "Bytes should be ~a" expected-bytes)))

;; Test cases
(test-wc "hello world" 0 2 11)
(test-wc "hello\nworld" 1 2 11)
(test-wc "the quick brown fox" 0 4 19)
(test-wc "line1\nline2\nline3\n" 3 3 18)
(test-wc "" 0 0 0)
(test-wc "   " 0 0 3)
(test-wc "word" 0 1 4)

;; Test with multiple spaces
(define result (word-count-sequential "hello    world"))
(check-equal? result '(0 2 14) "Multiple spaces should be handled")

;; Test sequential vs parallel equivalence
(define test-text (generate-text 10000 42))
(define seq-result (word-count-sequential test-text))
(define par-result (word-count-parallel test-text 4))

(check-equal? seq-result par-result
              "Sequential and parallel results should match")

