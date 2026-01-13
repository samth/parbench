#lang racket

(require rackunit
         "../../benchmarks/mpl/centrality.rkt")

;; Centrality computes betweenness centrality scores, not distances
;; Just verify functions run without error and produce vectors

(define test-graph (generate-random-graph 20 40 42))
(define seq-cent (centrality-sequential test-graph 0))
(define par-cent (centrality-parallel test-graph 0 4))

;; Check that results are vectors of correct length
(check-equal? (vector-length seq-cent) 20)
(check-equal? (vector-length par-cent) 20)

