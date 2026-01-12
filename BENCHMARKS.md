# Benchmark Guide

## Quick Start

The simplest way to run benchmarks is with `raco parbench` (or equivalently `./bench`):

```bash
# First install the package to enable raco parbench
raco pkg install --link .

raco parbench fib          # Run fib benchmark, print summary table
raco parbench mpl          # Run all MPL benchmarks (27)
raco parbench shootout     # Run Shootout benchmarks (6)
raco parbench racket       # Run Racket benchmarks (3)
raco parbench --quick      # Quick smoke test (3 iterations)
raco parbench -v fib       # Verbose output
raco parbench --save fib   # Save log files
raco parbench --html fib   # Save logs and generate HTML report
raco parbench --iterations 5  # Set iteration count
raco parbench --cores 1,4,8   # Specific core counts
raco parbench --help       # Full options
```

By default, `raco parbench` runs quietly and prints a summary table without saving files.

## Suite Runners

For more control, use the suite-specific runners directly:

```bash
# MPL benchmarks (27) with custom worker counts
racket run-mpl-benchmarks.rkt --workers 1,2,4,8 --log-dir results/mpl

# Shootout benchmarks (6)
racket run-shootout-benchmarks.rkt --workers 1,4,8 --output shootout.html

# Racket benchmarks (3)
racket run-racket-benchmarks.rkt --workers 1,2,4
```

Each runner:
- Runs all benchmarks in the suite
- Sweeps through specified worker counts
- Generates an HTML visualization report

## Individual Benchmarks

Run any single benchmark directly:

```bash
# MPL benchmarks
racket benchmarks/mpl/fib.rkt --n 42 --threshold 30 --workers 4 --repeat 5
racket benchmarks/mpl/histogram.rkt --n 200000000 --workers 8 --log results/hist.sexp
racket benchmarks/mpl/bfs.rkt --n 8000000 --graph-type grid --workers 4

# Shootout benchmarks
racket benchmarks/shootout/binary-trees.rkt --n 18 --workers 8 --repeat 10
racket benchmarks/shootout/mandelbrot.rkt --n 4000 --workers 8

# Racket benchmarks
racket benchmarks/racket/bmbench.rkt --n 1000000 --workers 4 --repeat 10
racket benchmarks/racket/richards.rkt --iterations 100 --workers 8
```

### Common Options

All benchmarks support:
- `--workers N` - Number of parallel workers
- `--repeat N` - Number of timed iterations
- `--log FILE` - Write S-expression results to file
- `--skip-sequential` - Skip sequential baseline run

## Log Format

Each run produces an S-expression record:

```scheme
(benchmark
  (name histogram)
  (variant parallel)
  (iteration 1)
  (repeat 10)
  (metrics (cpu-ms 520) (real-ms 515) (gc-ms 12))
  (params (n 200000000) (workers 8))
  (metadata (timestamp 1758661801) (racket-version "8.18"))
  (status ok))
```

## Analysis Tools

```bash
# Summarize log files (mean, stddev, min, max)
racket benchmarks/tools/summarize-results.rkt results/*.sexp

# Generate PNG plots
racket benchmarks/tools/plot-results.rkt \
  --input results/*.sexp \
  --metric real \
  --output plots/benchmark.png

# Interactive HTML dashboard
racket benchmarks/tools/visualize.rkt \
  --log-dir results \
  --output dashboard.html
```

## Configuration Files

Pre-defined configurations in `benchmarks/config/`:

| File | Purpose |
|------|---------|
| `quick.sexp` | Fast smoke tests (small sizes, 1 repeat) |
| `standard.sexp` | Typical benchmarking (moderate sizes) |
| `stress.sexp` | Large problems (comprehensive evaluation) |

Use with the suite runner:
```bash
racket benchmarks/run-suite.rkt --suite all --config benchmarks/config/quick.sexp
```

## Benchmark Reference

### MPL Benchmarks (27)

| Category | Benchmarks |
|----------|-----------|
| Graph | bfs, mis, msf, connectivity, triangle-count, centrality, convex-hull |
| Sorting | integer-sort, merge-sort, samplesort, suffix-array |
| Numeric | histogram, primes, fib, nqueens, mcss, subset-sum, bignum-add |
| Text | tokens, word-count, grep, dedup, palindrome, parens |
| Other | flatten, collect, shuffle |

### Shootout Benchmarks (6)

binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna

### Racket Benchmarks (3)

- **bmbench** - Boyer-Moore majority voting
- **richards** - Richards device scheduler
- **rows1b** - Synthetic row processing workload
