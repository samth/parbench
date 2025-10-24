;; Scaling configuration for merge-sort across worker counts.

((overrides
  (merge-sort-workers-1
   (--n "1000000" --seed "42" --threshold "2000" --workers "1" --repeat "3"))
  (merge-sort-workers-2
   (--n "1000000" --seed "42" --threshold "2000" --workers "2" --repeat "3"))
  (merge-sort-workers-3
   (--n "1000000" --seed "42" --threshold "2000" --workers "3" --repeat "3"))
  (merge-sort-workers-4
   (--n "1000000" --seed "42" --threshold "2000" --workers "4" --repeat "3"))
  (merge-sort-workers-5
   (--n "1000000" --seed "42" --threshold "2000" --workers "5" --repeat "3"))
  (merge-sort-workers-6
   (--n "1000000" --seed "42" --threshold "2000" --workers "6" --repeat "3"))
  (merge-sort-workers-7
   (--n "1000000" --seed "42" --threshold "2000" --workers "7" --repeat "3"))
  (merge-sort-workers-8
   (--n "1000000" --seed "42" --threshold "2000" --workers "8" --repeat "3"))))
