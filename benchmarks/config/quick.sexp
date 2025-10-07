;; Quick smoke test configuration
;; Runs all benchmarks with minimal problem sizes for fast validation

((overrides
  (bmbench (--sizes "1000" --workers "2" --repeat "1"))
  (bmbench-improved (--sizes "1000" --workers "2" --repeat "1"))
  (richards (--iterations "2" --workers "2" --repeat "1"))
  (rows1b (--rows "100000" --workers "2" --chunk-size "10000" --repeat "1"))
  (spectral-norm (--n "100" --iterations "5" --workers "2" --repeat "1"))
  (binary-trees (--max-depth "10" --workers "2" --repeat "1"))
  (nbody (--n "10000" --workers "2" --repeat "1"))
  (fannkuch-redux (--n "8" --workers "1" --repeat "1"))
  (mandelbrot (--n "200" --workers "2" --repeat "1"))
  (chameneos (--n "1000" --repeat "1"))
  (histogram (--n "100000" --buckets "128" --workers "2" --repeat "1"))
  (integer-sort (--n "100000" --range "10000" --workers "2" --repeat "1"))
  (bfs (--n "1000" --edge-prob "0.01" --source "0" --workers "2" --repeat "1"))))
