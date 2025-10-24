;; Scaling configuration for fib benchmark across worker counts.
;; Overrides ensure each run uses the same workload parameters.

((overrides
  (fib-workers-1 (--n "42" --threshold "20" --workers "1" --repeat "3"))
  (fib-workers-2 (--n "42" --threshold "20" --workers "2" --repeat "3"))
  (fib-workers-3 (--n "42" --threshold "20" --workers "3" --repeat "3"))
  (fib-workers-4 (--n "42" --threshold "20" --workers "4" --repeat "3"))
  (fib-workers-5 (--n "42" --threshold "20" --workers "5" --repeat "3"))
  (fib-workers-6 (--n "42" --threshold "20" --workers "6" --repeat "3"))
  (fib-workers-7 (--n "42" --threshold "20" --workers "7" --repeat "3"))
  (fib-workers-8 (--n "42" --threshold "20" --workers "8" --repeat "3"))))
