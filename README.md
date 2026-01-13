# Parallel Benchmark Suite

A comprehensive benchmarking suite for evaluating parallel performance in Racket.

[![CI](https://github.com/samth/parbench/actions/workflows/ci.yml/badge.svg)](https://github.com/samth/parbench/actions/workflows/ci.yml)
[![Racket Package](https://img.shields.io/badge/raco%20pkg-parbench-blue)](https://pkgs.racket-lang.org/package/parbench)
[![Documentation](https://img.shields.io/badge/docs-racket--lang.org-blue)](https://docs.racket-lang.org/parbench/)

## Installation

Install from the Racket package catalog:

```bash
raco pkg install parbench
```

Or install from source:

```bash
git clone https://github.com/samth/parbench.git
cd parbench
raco pkg install --link .
```

After installation, use `raco parbench` to run benchmarks.

**Documentation:** [docs.racket-lang.org/parbench](https://docs.racket-lang.org/parbench/)

## Quick Start

```bash
# Run a benchmark (prints summary table)
raco parbench fib

# Run specific suite
raco parbench mpl          # 27 MPL parallel benchmarks
raco parbench shootout     # 6 Shootout benchmarks
raco parbench racket       # 3 Racket benchmarks

# Quick smoke test (3 iterations)
raco parbench --quick

# Verbose output
raco parbench -v fib

# Save results to log files
raco parbench --save fib

# Save logs and generate HTML report
raco parbench --html fib

# Set number of iterations
raco parbench --iterations 5 fib

# Scale problem sizes (for quick testing)
raco parbench --work 0.1 fib       # 10% of normal size
raco parbench --work 0.001 fib     # Very small for smoke tests

# Specific core counts
raco parbench --cores 1,4,8
raco parbench mpl --cores 1-8

# View options
raco parbench --help
raco parbench --list
```

By default, `raco parbench` runs quietly and prints a summary table without saving files.
Use `--save` to save log files or `--html` to also generate HTML reports.

Alternatively, run `./bench` directly from the repository root with the same arguments.

### Example Output

```
$ raco parbench --quick fib
Parbench (quick mode)

Running mpl benchmarks...
  fib

========================================
  Results Summary
========================================

                              seq                  1 workers               4 workers
Benchmark               mean/median/min         mean/median/min         mean/median/min
--------------------------------------------------------------------------------------------
fib                     785.0/788.0/764         798.0/797.0/795         216.0/217.0/209
```

## Benchmark Suites

| Suite | Benchmarks | Description |
|-------|------------|-------------|
| **mpl** | 27 | Graph algorithms, sorting, numeric, text processing |
| **shootout** | 6 | Classic language benchmark game workloads |
| **racket** | 3 | Boyer-Moore, Richards, synthetic workloads |

### MPL Benchmarks (27)
Graph: bfs, mis, msf, connectivity, triangle-count, centrality, convex-hull
Sorting: integer-sort, merge-sort, samplesort, suffix-array
Numeric: histogram, primes, fib, nqueens, mcss, subset-sum, bignum-add
Text: tokens, word-count, grep, dedup, palindrome, parens
Other: flatten, collect, shuffle

### Shootout Benchmarks (6)
binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna

### Racket Benchmarks (3)
bmbench (Boyer-Moore majority), richards (OS simulator), rows1b (data processing)

## Requirements

- Racket 9.0+
- 4+ CPU cores recommended
- Linux or macOS (Windows via WSL)

## Running Individual Benchmarks

```bash
# Run a single benchmark directly
racket -l parbench/benchmarks/mpl/fib -- --n 42 --workers 4 --repeat 3

# With logging
racket -l parbench/benchmarks/mpl/histogram -- --n 10000000 --workers 8 --log results/hist.sexp
```

The `--` separator tells `racket` that subsequent arguments should be passed to the module.

## Analyzing Results

```bash
# Summarize log files
racket -l parbench/benchmarks/tools/summarize-results -- results/*.sexp

# Generate plots
racket -l parbench/benchmarks/tools/plot-results -- --input results/*.sexp --output plot.png
```

## Testing

```bash
raco test tests/
```

## Documentation

- **[Online Documentation](https://docs.racket-lang.org/parbench/)** - Full reference on docs.racket-lang.org
- **[BENCHMARKS.md](BENCHMARKS.md)** - Detailed usage guide and CLI reference
- **[docs/PERFORMANCE.md](docs/PERFORMANCE.md)** - Performance analysis and findings

## Repository Structure

```
bench                 # Unified benchmark runner
run-*-benchmarks.rkt  # Suite-specific runners
benchmarks/
  common/             # Shared CLI, logging infrastructure
  mpl/                # MPL parallel algorithms (27)
  shootout/           # Shootout benchmarks (6)
  racket/             # Racket benchmarks (3)
  tools/              # Analysis and visualization
  config/             # Configuration files
tests/                # RackUnit test suite
docs/                 # Additional documentation and analysis
```

## License

Apache 2.0 or MIT, at your option. See [LICENSE](LICENSE).
