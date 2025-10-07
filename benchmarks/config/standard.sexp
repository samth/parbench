;; Standard configuration
;; Moderate problem sizes for typical benchmarking runs

((overrides
  (bmbench (--sizes "20000,50000" --workers "4,8" --repeat "5"))
  (bmbench-improved (--sizes "20000,50000" --workers "4,8" --repeat "5"))
  (richards (--iterations "10" --workers "4,8" --repeat "5"))
  (rows1b (--rows "5000000" --workers "8" --chunk-size "500000" --repeat "5"))
  (spectral-norm (--n "2000" --iterations "15" --workers "8" --repeat "5"))
  (binary-trees (--max-depth "16" --workers "8" --repeat "5"))
  (nbody (--n "1000000" --workers "8" --repeat "5"))
  (fannkuch-redux (--n "10" --workers "1" --repeat "5"))
  (mandelbrot (--n "1000" --workers "8" --repeat "5"))
  (chameneos (--n "10000" --repeat "5"))
  (histogram (--n "10000000" --buckets "256" --workers "8" --repeat "5"))
  (integer-sort (--n "10000000" --range "1000000" --workers "8" --repeat "5"))
  (bfs (--n "10000" --edge-prob "0.001" --source "0" --workers "8" --repeat "5"))))
