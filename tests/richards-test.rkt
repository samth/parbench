#lang racket

(require rackunit
         "../benchmarks/racket/richards.rkt")

(module+ test
  (test-case "richards sequential baseline"
    (define res (run-richards-sequential #:iterations 1))
    (check-equal? (richards-result-queue-count res) 23246)
    (check-equal? (richards-result-hold-count res) 9297))

  (test-case "richards sequential aggregates"
    (define res (run-richards-sequential #:iterations 5))
    (check-equal? (richards-result-queue-count res) (* 5 23246))
    (check-equal? (richards-result-hold-count res) (* 5 9297)))

  (test-case "richards parallel matches sequential"
    (define seq-res (run-richards-sequential #:iterations 8))
    (define par-res (run-richards-parallel #:iterations 8 #:workers 4))
    (check-equal? (richards-result-queue-count par-res)
                  (richards-result-queue-count seq-res))
    (check-equal? (richards-result-hold-count par-res)
                  (richards-result-hold-count seq-res))))
