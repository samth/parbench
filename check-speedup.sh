#!/bin/bash

for bench in binary-trees spectral-norm fannkuch-redux mandelbrot fasta regex-dna k-nucleotide nbody; do
  echo "=== $bench ==="
  seq_time=$(grep "variant sequential" logs/shootout/$bench.sexp | grep "real-ms" | head -1 | grep -oP 'real-ms \K[0-9]+')
  par_time=$(grep "variant parallel" logs/shootout/$bench.sexp | grep "workers 8" | grep "real-ms" | head -1 | grep -oP 'real-ms \K[0-9]+')
  if [ -n "$seq_time" ] && [ -n "$par_time" ]; then
    speedup=$(echo "scale=2; $seq_time / $par_time" | bc)
    echo "Sequential: ${seq_time}ms, Parallel (8 workers): ${par_time}ms, Speedup: ${speedup}x"
  fi
done
