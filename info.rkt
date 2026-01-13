#lang info

(define collection "parbench")
(define version "0.1")
(define deps '(("base" #:version "9.0")
               "plot-lib"))
(define build-deps '("rackunit-lib" "recspecs-lib" "at-exp-lib"))

(define license
  '(Apache-2.0 OR MIT))

(define raco-commands
  '(("parbench" (submod parbench/raco-parbench main) "run parallel benchmarks" #f)))
