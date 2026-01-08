# Parallel Benchmark Suite

A comprehensive benchmarking suite for evaluating parallel performance in Racket.

## Quick Start

```bash
# Run all benchmarks (auto-detects cores, sweeps worker counts)
./bench

# Run specific suite
./bench mpl          # 27 MPL parallel benchmarks
./bench shootout     # 12 Shootout benchmarks
./bench racket       # 3 Racket benchmarks

# Quick smoke test (small sizes, faster)
./bench --quick

# Specific core counts
./bench --cores 1,4,8
./bench mpl --cores 1-8

# View options
./bench --help
./bench --list
```

Results are saved to `./results/` with HTML reports for visualization.

## Benchmark Suites

| Suite | Benchmarks | Description |
|-------|------------|-------------|
| **mpl** | 27 | Graph algorithms, sorting, numeric, text processing |
| **shootout** | 12 | Classic language benchmark game workloads |
| **racket** | 3 | Boyer-Moore, Richards, synthetic workloads |

### MPL Benchmarks (27)
Graph: bfs, mis, msf, connectivity, triangle-count, centrality, convex-hull
Sorting: integer-sort, merge-sort, samplesort, suffix-array
Numeric: histogram, primes, fib, nqueens, mcss, subset-sum, bignum-add
Text: tokens, word-count, grep, dedup, palindrome, parens
Other: flatten, collect, shuffle

### Shootout Benchmarks (12)
binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna
(Each has v1 and v2 parallel implementations)

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
- **[PERFORMANCE.md](PERFORMANCE.md)** - Performance analysis and findings
- **[CLAUDE.md](CLAUDE.md)** - AI assistant guidelines

## Repository Structure

```
bench                 # Unified benchmark runner
run-*-benchmarks.rkt  # Suite-specific runners
benchmarks/
  common/             # Shared CLI, logging infrastructure
  mpl/                # MPL parallel algorithms (27)
  shootout/           # Shootout benchmarks (12)
  racket/             # Racket benchmarks (3)
  tools/              # Analysis and visualization
  config/             # Configuration files
tests/                # RackUnit test suite
```

## License

Apache 2.0 or MIT, at your option. See [LICENSE](LICENSE).
