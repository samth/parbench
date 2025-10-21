#lang racket

(require rackunit
         "../../benchmarks/mpl/tokens.rkt")

;; Test tokenization with known inputs
(define (test-tokens text expected)
  (define result (tokens-sequential text))
  (check-equal? result expected
                (format "Tokenization should match")))

;; Test cases
(test-tokens "hello world" '("hello" "world"))
(test-tokens "the quick brown fox" '("the" "quick" "brown" "fox"))
(test-tokens "  spaces   everywhere  " '("spaces" "everywhere"))
(test-tokens "one" '("one"))
(test-tokens "" '())
(test-tokens "   " '())

;; Test with tabs and newlines
(define result (tokens-sequential "hello\tworld\nfoo\rbar"))
(check-equal? result '("hello" "world" "foo" "bar")
              "Should handle tabs, newlines, and carriage returns")

;; Test sequential vs parallel equivalence
(define test-text (generate-text 10000 42))
(define seq-tokens (tokens-sequential test-text))
(define par-tokens (tokens-parallel test-text 4))

(check-equal? (length seq-tokens) (length par-tokens)
              "Sequential and parallel should produce same number of tokens")

;; Verify tokens match (they might be in different order due to chunking,
;; but for our generated text with spaces, they should match)
(check-equal? seq-tokens par-tokens
              "Sequential and parallel tokens should match exactly")

(printf "tokens tests passed\n")
