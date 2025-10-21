#lang racket

(require rackunit
         "../benchmarks/shootout/k-nucleotide.rkt")

(module+ test
  (define expected
    '((20 (("C" . 6)
           ("G" . 6)
           ("A" . 4)
           ("T" . 3)
           ("B" . 1)))
      (19 (("AG" . 3)
           ("CC" . 3)
           ("GC" . 3)
           ("GA" . 2)
           ("BT" . 1)
           ("CA" . 1)
           ("CG" . 1)
           ("CT" . 1)
           ("GG" . 1)
           ("TA" . 1)
           ("TB" . 1)
           ("TT" . 1)))
      (("GGT" 0)
       ("GGTA" 0)
       ("GGTATT" 0)
       ("GGTATTTTAATT" 0)
       ("GGTATTTTAATTTATAGT" 0))))
  (check-equal? (k-nucleotide 2 #:workers 1) expected)
  (check-equal? (k-nucleotide 2 #:workers 4) expected))
