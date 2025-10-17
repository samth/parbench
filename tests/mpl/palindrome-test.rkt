#lang racket

(require rackunit
         "../../benchmarks/mpl/palindrome.rkt")

(module+ test
  ;; Test known palindromes
  (check-equal? (palindrome-sequential "racecar") #t)
  (check-equal? (palindrome-sequential "abba") #t)
  (check-equal? (palindrome-sequential "a") #t)
  (check-equal? (palindrome-sequential "") #t)

  ;; Test non-palindromes
  (check-equal? (palindrome-sequential "hello") #f)
  (check-equal? (palindrome-sequential "racecars") #f)

  ;; Test parallel version matches sequential
  (define test-str (generate-test-string 10000 #t 42))
  (define seq (palindrome-sequential test-str))
  (define par (palindrome-parallel test-str 3 1000))
  (check-equal? par seq)
  (check-equal? par #t))
