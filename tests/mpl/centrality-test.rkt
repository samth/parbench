#lang racket

(require rackunit
         "../../benchmarks/mpl/centrality.rkt")

;; Test with simple graphs
;; Star graph: center connected to all others
(define star-graph
  (vector '(1 2 3 4)  ; 0 is center
          '(0)        ; 1 connected to center
          '(0)        ; 2 connected to center
          '(0)        ; 3 connected to center
          '(0)))      ; 4 connected to center

(define star-result (centrality-sequential star-graph 0))
(check-equal? (vector-ref star-result 0) 0.0 "Source has distance 0")
(check-equal? (vector-ref star-result 1) 1.0 "Distance 1 from center")
(check-equal? (vector-ref star-result 4) 1.0 "All spokes equidistant")

;; Line graph: 0-1-2-3-4
(define line-graph
  (vector '(1)
          '(0 2)
          '(1 3)
          '(2 4)
          '(3)))

(define line-result (centrality-sequential line-graph 0))
(check-equal? (vector-ref line-result 0) 0.0 "Source")
(check-equal? (vector-ref line-result 1) 1.0 "Distance 1")
(check-equal? (vector-ref line-result 2) 2.0 "Distance 2")
(check-equal? (vector-ref line-result 3) 3.0 "Distance 3")
(check-equal? (vector-ref line-result 4) 4.0 "Distance 4")

;; Disconnected graph
(define disconnected-graph
  (vector '(1)     ; Component 1: 0-1
          '(0)
          '(3)     ; Component 2: 2-3
          '(2)))

(define disc-result (centrality-sequential disconnected-graph 0))
(check-equal? (vector-ref disc-result 0) 0.0 "Source")
(check-equal? (vector-ref disc-result 1) 1.0 "Reachable")
(check-equal? (vector-ref disc-result 2) 0.0 "Unreachable stays 0")
(check-equal? (vector-ref disc-result 3) 0.0 "Unreachable stays 0")

;; Test sequential vs parallel
(define test-graph (generate-random-graph 50 100 42))
(define seq-cent (centrality-sequential test-graph 0))
(define par-cent (centrality-parallel test-graph 0 4))
(check-equal? seq-cent par-cent
              "Sequential and parallel should match")

(printf "centrality tests passed\n")
