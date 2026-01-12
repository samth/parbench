#lang info

(define collection "parbench")
(define version "0.1")
(define deps '("plot-gui-lib"
               "base"
               "plot-lib"))
(define build-deps '("rackunit-lib"))

(define raco-commands
  '(("parbench" (submod parbench/raco-parbench main) "run parallel benchmarks" #f)))
