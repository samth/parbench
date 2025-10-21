#lang racket

(require rackunit
         "../../benchmarks/mpl/triangle-count.rkt")

;; Test with known graphs
;; Triangle: 0-1-2 all connected
(define triangle-graph
  (vector '(1 2)    ; 0 connected to 1, 2
          '(0 2)    ; 1 connected to 0, 2
          '(0 1)))  ; 2 connected to 0, 1

(check-equal? (triangle-count-sequential triangle-graph) 1
              "Single triangle should count as 1")

;; Two triangles sharing an edge
(define two-triangles
  (vector '(1 2 3)  ; 0
          '(0 2)    ; 1 - triangle 0-1-2
          '(0 1 3)  ; 2
          '(0 2)))  ; 3 - triangle 0-2-3

(check-equal? (triangle-count-sequential two-triangles) 2
              "Should find 2 triangles")

;; No triangles (line graph)
(define line-graph
  (vector '(1)
          '(0 2)
          '(1)))

(check-equal? (triangle-count-sequential line-graph) 0
              "Line graph has no triangles")

;; Complete graph K4 (4 vertices all connected)
;; Should have C(4,3) = 4 triangles
(define k4-graph
  (vector '(1 2 3)
          '(0 2 3)
          '(0 1 3)
          '(0 1 2)))

(check-equal? (triangle-count-sequential k4-graph) 4
              "K4 should have 4 triangles")

;; Test sequential vs parallel
(define test-graph (generate-random-graph 50 200 42))
(define seq-count (triangle-count-sequential test-graph))
(define par-count (triangle-count-parallel test-graph 4))
(check-equal? seq-count par-count
              "Sequential and parallel should count same triangles")

(printf "triangle-count tests passed\n")
