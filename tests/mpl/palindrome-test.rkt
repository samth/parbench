#lang racket

(require rackunit
         "../../benchmarks/mpl/palindrome.rkt")

(module+ test
  ;; Test with known patterns - functions return (values position length)

  ;; Test with "racecar" - the whole string is a palindrome
  (define-values (pos1 len1) (longest-palindrome-sequential "racecar"))
  (check-equal? len1 7)  ; "racecar" is 7 chars

  ;; Test with "abba" - the whole string is a palindrome
  (define-values (pos2 len2) (longest-palindrome-sequential "abba"))
  (check-equal? len2 4)

  ;; Test parallel version matches sequential
  (define test-str (generate-test-string 1000 42))
  (define-values (seq-pos seq-len) (longest-palindrome-sequential test-str))
  (define-values (par-pos par-len) (longest-palindrome-parallel test-str 4))
  (check-equal? par-len seq-len))
