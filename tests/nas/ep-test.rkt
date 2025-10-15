#lang racket

(require rackunit
         "../../benchmarks/nas/ep.rkt")

(define small-pairs 1024)

(define seq-result (ep #:class 'S #:pairs small-pairs #:workers 1))
(define par-result (ep #:class 'S #:pairs small-pairs #:workers 4))

(test-case "Sequential vs parallel agreement"
  (check-true (ep-stats≈ par-result seq-result)))

(test-case "Counts sum to total pairs"
  (define total (for/sum ([c (in-vector (ep-stats-counts seq-result))]) c))
  (check-equal? total small-pairs))

(test-case "Stats structure exposes sums"
  (check-true (real? (ep-stats-sx seq-result)))
  (check-true (real? (ep-stats-sy seq-result))))
