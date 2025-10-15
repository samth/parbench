#lang racket

(provide class-parameters
         ep-class->pairs
         is-class->params
         cg-class->params
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

;; IS kernel parameters: total keys, max key value, number of buckets, test iterations
(define is-class->params
  (hash 'S (hash 'total-keys (expt 2 16)
                 'max-key (expt 2 11)
                 'num-buckets 1024
                 'test-iterations 10)
        'W (hash 'total-keys (expt 2 20)
                 'max-key (expt 2 16)
                 'num-buckets 1024
                 'test-iterations 10)
        'A (hash 'total-keys (expt 2 23)
                 'max-key (expt 2 19)
                 'num-buckets 1024
                 'test-iterations 10)
        'B (hash 'total-keys (expt 2 25)
                 'max-key (expt 2 21)
                 'num-buckets 1024
                 'test-iterations 10)
        'C (hash 'total-keys (expt 2 27)
                 'max-key (expt 2 23)
                 'num-buckets 1024
                 'test-iterations 10)))

;; CG kernel parameters: matrix size, nonzeros per row, iterations, verification zeta
(define cg-class->params
  (hash 'S (hash 'na 1400 'nonzer 7 'niter 15 'shift 10.0
                 'rcond 0.1 'verify-zeta 8.5971775078648)
        'W (hash 'na 7000 'nonzer 8 'niter 15 'shift 12.0
                 'rcond 0.1 'verify-zeta 10.362595087124)
        'A (hash 'na 14000 'nonzer 11 'niter 15 'shift 20.0
                 'rcond 0.1 'verify-zeta 17.130235054029)
        'B (hash 'na 75000 'nonzer 13 'niter 75 'shift 60.0
                 'rcond 0.1 'verify-zeta 22.712745482631)
        'C (hash 'na 150000 'nonzer 15 'niter 75 'shift 110.0
                 'rcond 0.1 'verify-zeta 28.973605592845)))
