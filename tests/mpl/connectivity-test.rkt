#lang racket

(require rackunit
         "../../benchmarks/mpl/connectivity.rkt")

;; Test with simple graphs
;; Graph with 3 components: {0,1}, {2,3}, {4}
(define test-graph1
  (vector '(1)      ; 0 connected to 1
          '(0)      ; 1 connected to 0
          '(3)      ; 2 connected to 3
          '(2)      ; 3 connected to 2
          '()))     ; 4 isolated

(define uf1 (connectivity-sequential test-graph1))
(check-equal? (count-components uf1 5) 3
              "Should find 3 components")

;; Fully connected graph
(define test-graph2
  (vector '(1 2)
          '(0 2)
          '(0 1)))

(define uf2 (connectivity-sequential test-graph2))
(check-equal? (count-components uf2 3) 1
              "Fully connected should have 1 component")

;; All isolated vertices
(define test-graph3
  (vector '() '() '() '()))

(define uf3 (connectivity-sequential test-graph3))
(check-equal? (count-components uf3 4) 4
              "All isolated should have 4 components")

;; Test sequential vs parallel with small graph to avoid resource issues
(define random-graph (generate-random-graph 50 100 42))
(define seq-uf (connectivity-sequential random-graph))
(define par-uf (connectivity-parallel random-graph 2))
(check-equal? (count-components seq-uf 50)
              (count-components par-uf 50)
              "Sequential and parallel should find same number of components")

