;; Stress test configuration
;; Large problem sizes for comprehensive performance evaluation

((overrides
  (bmbench (--sizes "50000,100000,200000" --workers "1,4,8,16" --repeat "10"))
  (bmbench-improved (--sizes "50000,100000,200000" --workers "1,4,8,16" --repeat "10"))
  (richards (--iterations "20" --workers "1,4,8,16" --repeat "10"))
  (rows1b (--rows "10000000" --workers "16" --chunk-size "500000" --repeat "10"))
  (spectral-norm (--n "5000" --iterations "20" --workers "16" --repeat "10"))
  (binary-trees (--max-depth "20" --workers "16" --repeat "10"))
  (nbody (--n "5000000" --workers "16" --repeat "10"))
  (fannkuch-redux (--n "11" --workers "1" --repeat "10"))
  (mandelbrot (--n "4000" --workers "16" --repeat "10"))
  (chameneos (--n "50000" --repeat "10"))))
