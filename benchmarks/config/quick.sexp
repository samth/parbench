;; Quick smoke test configuration
;; Runs all benchmarks with minimal problem sizes for fast validation

((overrides
  (bmbench (--sizes "200" --workers "2" --repeat "1"))
  (bmbench-improved (--sizes "200" --workers "2" --repeat "1"))
  (richards (--iterations "1" --workers "2" --repeat "1"))
  (rows1b (--rows "5000" --workers "2" --chunk-size "2000" --repeat "1"))
  (spectral-norm (--n "30" --iterations "2" --workers "2" --repeat "1"))
  (binary-trees (--max-depth "7" --workers "2" --repeat "1"))
  (nbody (--n "1000" --workers "2" --repeat "1"))
  (fannkuch-redux (--n "6" --workers "1" --repeat "1"))
  (mandelbrot (--n "120" --workers "2" --repeat "1"))
  (chameneos (--n "300" --repeat "1"))
  (histogram (--n "10000" --buckets "64" --workers "2" --repeat "1"))
  (integer-sort (--n "10000" --range "2000" --workers "2" --repeat "1"))
  (bfs (--n "200" --edge-prob "0.03" --source "0" --workers "2" --repeat "1"))
  (mis (--n "150" --degree "3" --workers "2" --repeat "1"))
  (msf (--n "80" --degree "3" --workers "2" --repeat "1"))
  (suffix-array (--n "1000" --alphabet "4" --workers "2" --repeat "1"))
  (convex-hull (--n "200" --distribution "uniform-circle" --workers "2" --repeat "1"))))
