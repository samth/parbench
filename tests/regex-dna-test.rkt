#lang racket

(require rackunit
         "../benchmarks/shootout/regex-dna.rkt")

(module+ test
  (define expected-variants
    '(("AGGGTAAA|TTTACCCT" . 0)
      ("[CGT]GGGTAAA|TTTACCC[ACG]" . 0)
      ("A[ACT]GGTAAA|TTTACC[AGT]T" . 0)
      ("AG[ACT]GTAAA|TTTAC[AGT]CT" . 0)
      ("AGG[ACT]TAAA|TTTA[AGT]CCT" . 0)
      ("AGGG[ACG]AAA|TTT[CGT]CCCT" . 0)
      ("AGGGT[CGT]AA|TT[ACG]ACCCT" . 0)
      ("AGGGTA[CGT]A|T[ACG]TACCCT" . 0)
      ("AGGGTAA[CGT]|[ACG]TTACCCT" . 0)
      ("AGGGTAAC|GTTACCCT" . 0)))
  (define expected (list expected-variants 20 26))
  (check-equal? (regex-dna 2 #:workers 1) expected)
  (check-equal? (regex-dna 2 #:workers 3) expected))
