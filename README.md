# Parallel Benchmark Suite

A comprehensive benchmarking suite for evaluating parallel performance in Racket.

## Quick Start

```bash
# Install the package (enables raco parbench command)
raco pkg install --link .

# Run a benchmark (prints summary table)
raco parbench fib
# Or equivalently: ./bench fib

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

# Specific core counts
raco parbench --cores 1,4,8
raco parbench mpl --cores 1-8

# View options
raco parbench --help
raco parbench --list
```

By default, `raco parbench` runs quietly and prints a summary table without saving files.
Use `--save` to save log files or `--html` to also generate HTML reports.

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

- Racket 8.10+
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

- **[BENCHMARKS.md](BENCHMARKS.md)** - Detailed usage guide and CLI reference
- **[docs/PERFORMANCE.md](docs/PERFORMANCE.md)** - Performance analysis and findings
- **[CLAUDE.md](CLAUDE.md)** - AI assistant guidelines

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
