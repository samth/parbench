#lang info

(define collection "parbench")
(define version "0.1")
(define deps '("base"
               "plot-lib"))
(define build-deps '("rackunit-lib" "recspecs" "recspecs-lib" "at-exp-lib"))

(define raco-commands
  '(("parbench" (submod parbench/raco-parbench main) "run parallel benchmarks" #f)))
