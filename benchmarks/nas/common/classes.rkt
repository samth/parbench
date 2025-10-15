#lang racket

(provide class-parameters
         ep-class->pairs
         default-nas-class)

;; NAS problem classes in increasing order of difficulty.
(define class-parameters
  (hash 'S (hash 'name "Small" 'rank 0)
        'W (hash 'name "Test/Workload" 'rank 1)
        'A (hash 'name "Class A" 'rank 2)
        'B (hash 'name "Class B" 'rank 3)
        'C (hash 'name "Class C" 'rank 4)))

(define default-nas-class 'S)

;; EP kernel uses 2^M Gaussian pairs, where M depends on the class.
;; Values derived from the NAS Parallel Benchmarks 3.4 specification.
(define ep-class->pairs
  (hash 'S (expt 2 24)
        'W (expt 2 25)
        'A (expt 2 28)
        'B (expt 2 30)
        'C (expt 2 32)))
