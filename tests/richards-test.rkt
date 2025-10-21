#lang racket

(require rackunit
         "../benchmarks/racket/richards.rkt")

(module+ test
  (test-case "richards sequential baseline"
    (define res (run-richards-sequential #:iterations 1))
    (check-equal? (richards-result-queue-count res) 23246)
    (check-equal? (richards-result-hold-count res) 9297))

  (test-case "richards sequential aggregates"
    (define res (run-richards-sequential #:iterations 3))
    (check-equal? (richards-result-queue-count res) (* 3 23246))
    (check-equal? (richards-result-hold-count res) (* 3 9297)))

  (test-case "richards parallel matches sequential"
    (define seq-res (run-richards-sequential #:iterations 4))
    (define par-res (run-richards-parallel #:iterations 4 #:workers 4))
    (check-equal? (richards-result-queue-count par-res)
                  (richards-result-queue-count seq-res))
    (check-equal? (richards-result-hold-count par-res)
                  (richards-result-hold-count seq-res))))
