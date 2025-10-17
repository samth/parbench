#lang racket

(require rackunit
         "../benchmarks/racket/bmbench_improved.rkt")

(module+ test
  (test-case "improved parallel matches sequential on small vector"
    (define vec #(1 2 1 1 3 1 4 1 5 1))
    (define seq (vector-boyer-moore-majority/sequential vec))
    (define par (vector-boyer-moore-majority/parallel/improved vec #:workers 4 #:threshold 0))
    (check-equal? par seq))

  (test-case "improved parallel matches sequential on generated data"
    (random-seed 123)
    (define vec (make-majority-vector 5000 7 #:p 0.55 #:kinds 32))
    (define seq (vector-boyer-moore-majority/sequential vec))
    (define par (vector-boyer-moore-majority/parallel/improved vec #:workers 6 #:threshold 2000 #:chunk-multiplier 3))
    (check-equal? par seq)))
