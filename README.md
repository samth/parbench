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
./bench fib

# Run specific suite
./bench mpl          # 27 MPL parallel benchmarks
./bench shootout     # 6 Shootout benchmarks
./bench racket       # 3 Racket benchmarks

# Quick smoke test (3 iterations)
./bench --quick

# Verbose output
./bench -v fib

# Save results to log files
./bench --save fib

# Save logs and generate HTML report
./bench --html fib

# Set number of iterations
./bench --iterations 5 fib

# Scale problem sizes (for quick testing)
./bench --work 0.1 fib       # 10% of normal size
./bench --work 0.001 fib     # Very small for smoke tests

# Specific core counts
./bench --cores 1,4,8
./bench mpl --cores 1-8

# View options
./bench --help
./bench --list
```

By default, `./bench` runs quietly and prints a summary table without saving files.
Use `--save` to save log files or `--html` to also generate HTML reports.

After installing the package, `raco parbench` accepts all the same arguments.

### Example Output

```
$ ./bench --quick fib
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
racket benchmarks/mpl/fib.rkt --n 42 --workers 4 --repeat 3

# With logging
racket benchmarks/mpl/histogram.rkt --n 10000000 --workers 8 --log results/hist.sexp
```

## Analyzing Results

```bash
# Summarize log files
racket benchmarks/tools/summarize-results.rkt results/*.sexp

# Generate plots
racket benchmarks/tools/plot-results.rkt --input results/*.sexp --output plot.png
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
