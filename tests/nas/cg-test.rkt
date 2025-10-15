#lang racket

(require rackunit
         "../../benchmarks/nas/cg.rkt")

(test-case "Sequential CG benchmark runs"
  (define result (cg #:class 'S #:niter-override 5 #:workers 1))
  (check-true (cg-result? result))
  (check-true (real? (cg-result-zeta result))))

(test-case "Parallel CG benchmark runs"
  (define result (cg #:class 'S #:niter-override 5 #:workers 4))
  (check-true (cg-result? result))
  (check-true (real? (cg-result-zeta result))))

(test-case "Sequential vs parallel approximate agreement"
  (define seq-result (cg #:class 'S #:niter-override 5 #:workers 1))
  (define par-result (cg #:class 'S #:niter-override 5 #:workers 4))
  ;; CG may have small numerical differences due to floating point
  (define diff (abs (- (cg-result-zeta seq-result) (cg-result-zeta par-result))))
  (check-true (< diff 1e-6)))

(test-case "Result structure is correct"
  (define result (cg #:class 'S #:niter-override 5 #:workers 1))
  (check-equal? (cg-result-iterations result) 5))
