# Parallel Benchmark Suite

A comprehensive, reproducible benchmarking harness for evaluating parallel performance in Racket. This suite unifies existing Racket benchmarks with external parallel benchmark suites, providing consistent CLI interfaces, S-expression logging, and comparative reporting.

## Overview

This repository provides:
- **Racket Benchmarks** (3): Boyer-Moore, Richards, and synthetic workloads with sequential and parallel (futures/threads) variants
- **Shootout Benchmarks** (8): Classic language benchmark game workloads adapted for parallel execution (spectral-norm, binary-trees, nbody, fannkuch-redux, mandelbrot, fasta, regex-dna, k-nucleotide)
- **MPL Parallel Benchmarks** (27): Racket re-implementations of MPL benchmark algorithms covering graph algorithms, sorting, numeric computations, and more
- **Analysis Tools**: Log aggregation, statistical summaries, PNG plotting, and interactive HTML visualization dashboard

All benchmarks share a common infrastructure:
- Command-line argument parsing with `--log`, `--workers`, `--repeat` flags
- S-expression output format for machine-readable results
- Metadata capture (Racket version, timestamp, system info)
- RackUnit test coverage validating sequential vs. parallel correctness

## Environment Expectations

- Racket 8.10+ already installed and available via `racket`/`raco`
- Git for cloning/updating the repository
- Hardware with at least 4 cores if you want meaningful parallel scaling data
- Standard POSIX shell environment (Linux or macOS). Windows users should rely on WSL or equivalent.

## Getting Started

### Clone the repository

```bash
git clone https://github.com/your-username/parbench.git
cd parbench
```

### Run the test suite

```bash
raco test tests/
```

### Smoke-test a benchmark

```bash
racket benchmarks/racket/bmbench.rkt --sizes 1000 --workers 2 --repeat 1
```

### Create a log directory (optional)

```bash
mkdir -p logs

racket benchmarks/racket/bmbench.rkt --sizes 10000 --workers 4 --log logs/test.sexp
cat logs/test.sexp
```

### Troubleshooting

**Issue:** Benchmarks run slowly or don't show speedup
- **Solution:** Increase problem sizes and ensure sufficient CPU cores:
  ```bash
  # Check available cores
  nproc  # Linux
  sysctl -n hw.ncpu  # macOS
  ```

## Quick Start

### Running Individual Benchmarks

```bash
# Boyer-Moore majority benchmark (parallel futures)
racket benchmarks/racket/bmbench.rkt --sizes 20000 --workers 4 --log logs/bm.sexp

# Binary trees shootout benchmark
racket benchmarks/shootout/binary-trees.rkt --max-depth 16 --workers 8 --repeat 3

# N-body simulation
racket benchmarks/shootout/nbody.rkt --n 1000000 --workers 8 --log logs/nbody.sexp
```

### Summarizing Results

```bash
# Aggregate statistics from log files
racket benchmarks/tools/summarize-results.rkt logs/*.sexp

# Generate PNG plots
racket benchmarks/tools/plot-results.rkt \
  --input logs/binary-trees.sexp \
  --input logs/nbody.sexp \
  --metric real \
  --output plots/shootout.png
```

### Running Tests

```bash
# Run all unit tests
raco test tests/

# Run specific benchmark test
raco test tests/binary-trees-test.rkt
```

## Repository Structure

```
benchmarks/
  common/           # Shared CLI, logging, and harness modules
  racket/           # Native Racket benchmarks (Boyer-Moore, Richards, rows1b)
  shootout/         # Computer Language Benchmarks Game ports
  mpl/              # MPL parallel algorithm re-implementations
  tools/            # Analysis, summarization, and plotting utilities
tests/              # RackUnit test suite
```

## Documentation

- **[BENCHMARKS.md](BENCHMARKS.md)** - Detailed usage guide, CLI reference, and examples
- **[PLAN.md](PLAN.md)** - Development roadmap and implementation phases
- **[AGENTS.md](AGENTS.md)** - Contributor guidelines and repository structure expectations

## Benchmark Categories

### Racket Benchmarks
- **bmbench.rkt** - Boyer-Moore majority voting with parallel tree-merge reduction
- **richards.rkt** - OS simulator benchmark adapted from Martin Richards' original
- **rows1b.rkt** - Synthetic 1-billion-row data processing workload

### Shootout Benchmarks
- **spectral-norm.rkt** - Eigenvalue approximation via power iteration
- **binary-trees.rkt** - Binary tree allocation and traversal
- **nbody.rkt** - N-body gravitational simulation
- **fannkuch-redux.rkt** - Indexed access to permutations
- **mandelbrot.rkt** - Mandelbrot set fractal generation
- **fasta.rkt** - DNA sequence generation
- **regex-dna.rkt** - Regex pattern matching on DNA sequences
- **k-nucleotide.rkt** - K-mer frequency analysis

### MPL Parallel Benchmarks

**Graph Algorithms:**
- **bfs.rkt** - Breadth-first search (direction-optimizing)
- **mis.rkt** - Maximal Independent Set (Luby's algorithm)
- **msf.rkt** - Minimum Spanning Forest (Borůvka's algorithm)
- **connectivity.rkt** - Connected components
- **triangle-count.rkt** - Triangle counting
- **centrality.rkt** - Betweenness centrality (Brandes' algorithm)

**Sorting & Searching:**
- **integer-sort.rkt** - Parallel counting sort
- **merge-sort.rkt** - Parallel merge sort
- **samplesort.rkt** - Parallel sample sort
- **suffix-array.rkt** - Suffix array construction (prefix-doubling)

**Geometry:**
- **convex-hull.rkt** - Convex hull (parallel QuickHull)

**Numeric & Data Processing:**
- **histogram.rkt** - Parallel histogram with bucketed counting
- **primes.rkt** - Parallel prime sieve
- **fib.rkt** - Parallel Fibonacci
- **nqueens.rkt** - N-Queens solver

**Text & Sequence Processing:**
- **tokens.rkt** - Parallel tokenization
- **word-count.rkt** - Parallel word frequency counting
- **grep.rkt** - Parallel pattern search
- **dedup.rkt** - Parallel deduplication
- **palindrome.rkt** - Palindrome detection

**Collection Operations:**
- **mcss.rkt** - Maximum contiguous subsequence sum
- **flatten.rkt** - Parallel array flattening
- **collect.rkt** - Parallel filter/collect
- **bignum-add.rkt** - Big number addition
- **subset-sum.rkt** - Subset sum solver
- **shuffle.rkt** - Parallel Fisher-Yates shuffle

## Development Status

This is an active development project with most core features complete:
- ✅ Phase 1-2: Core infrastructure and logging (complete)
- ✅ Phase 3: Shootout benchmarks integration (8 benchmarks complete)
- ✅ Phase 4: Racket benchmarks (3 benchmarks complete)
- ✅ Phase 5: MPL benchmarks (27 benchmarks complete)
- ✅ Phase 6: Unified CLI orchestration (complete with config file support)
- ✅ Phase 7: Visualization and plotting tools (complete with interactive HTML dashboard)
- ⏳ CI integration (planned)

See [PLAN.md](PLAN.md) for detailed roadmap and progress tracking.

## Contributing

Contributions are welcome! When adding new benchmarks:
1. Follow the established CLI patterns (see `benchmarks/common/cli.rkt`)
2. Emit structured logs via `benchmarks/common/logging.rkt`
3. Provide both sequential and parallel variants where applicable
4. Add RackUnit tests to `tests/` validating correctness
5. Document usage in `BENCHMARKS.md`

See [AGENTS.md](AGENTS.md) for detailed contribution guidelines.

## License

This software is licensed under the Apache License, Version 2.0 or the MIT License, at your option. See [LICENSE](LICENSE) for details.
