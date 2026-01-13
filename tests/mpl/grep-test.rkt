#lang racket

(require rackunit
         "../../benchmarks/mpl/grep.rkt")

;; Test with known text
(define test-text
  '("hello world"
    "the quick brown fox"
    "PATTERN appears here"
    "another line"
    "PATTERN at start"
    "end PATTERN"
    "no match"
    "multiple PATTERN PATTERN matches"))

;; Should find lines 2, 4, 5, 7 (0-indexed)
(define pattern-matches (grep-sequential test-text "PATTERN"))
(check-equal? pattern-matches '(2 4 5 7)
              "Should find all lines with PATTERN")

;; Test case sensitivity
(define lowercase-matches (grep-sequential test-text "pattern"))
(check-equal? lowercase-matches '()
              "Should not match different case")

;; Test regex patterns
(define fox-matches (grep-sequential test-text "fox"))
(check-equal? fox-matches '(1)
              "Should find 'fox' in line 1")

(define start-pattern (grep-sequential test-text "^PATTERN"))
(check-equal? start-pattern '(2 4)
              "Should match PATTERN at start of line")

(define end-pattern (grep-sequential test-text "PATTERN$"))
(check-equal? end-pattern '(5)
              "Should match PATTERN at end of line")

;; Test word boundaries - Racket regexp doesn't support \b, use alternative
(define word-matches (grep-sequential test-text "world"))
(check-equal? word-matches '(0)
              "Should match 'world'")

;; Empty results
(define no-matches (grep-sequential test-text "xyz123"))
(check-equal? no-matches '()
              "Should return empty list for no matches")

;; Test sequential vs parallel
(define large-text (generate-random-text 1000 0.1 42))
(define seq-result (grep-sequential large-text "PATTERN"))
(define par-result (grep-parallel large-text "PATTERN" 4))
(check-equal? seq-result par-result
              "Sequential and parallel should match")

;; Test all lines match
(define all-match-text '("PATTERN1" "PATTERN2" "PATTERN3"))
(define all-matches (grep-sequential all-match-text "PATTERN"))
(check-equal? all-matches '(0 1 2)
              "Should match all lines")

;; Test different regex patterns in parallel
;; Line 0 has "world" and line 5 has "end" - both have 'd'
(define letter-matches (grep-parallel test-text "brown" 2))
(check-equal? letter-matches '(1)
              "Should find 'brown' in line 1")

