# Parallel Benchmark Suite

A comprehensive, reproducible benchmarking harness for evaluating parallel performance in Racket. This suite unifies existing Racket benchmarks with external parallel benchmark suites, providing consistent CLI interfaces, S-expression logging, and comparative reporting.

## Overview

This repository provides:
- **Racket Benchmarks**: Boyer-Moore, Richards, and synthetic workloads with sequential and parallel (futures/threads) variants
- **Shootout Benchmarks**: Classic language benchmark game workloads adapted for parallel execution (spectral-norm, binary-trees, n-body, fannkuch-redux, mandelbrot, chameneos)
- **NAS Parallel Benchmarks**: Wrapper infrastructure for running compiled NAS kernels with structured logging
- **Analysis Tools**: Log aggregation, statistical summaries, and visualization utilities

All benchmarks share a common infrastructure:
- Command-line argument parsing with `--log`, `--workers`, `--repeat` flags
- S-expression output format for machine-readable results
- Metadata capture (Racket version, timestamp, system info)
- RackUnit test coverage validating sequential vs. parallel correctness

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
  nas/              # NAS Parallel Benchmarks runner infrastructure
  tools/            # Analysis, summarization, and plotting utilities
tests/              # RackUnit test suite
```

## Documentation

- **[BENCHMARKS.md](BENCHMARKS.md)** - Detailed usage guide, CLI reference, and examples
- **[PLAN.md](PLAN.md)** - Development roadmap and implementation phases
- **[AGENTS.md](AGENTS.md)** - Contributor guidelines and repository structure expectations

## Benchmark Categories

### Racket Benchmarks
- **bmbench.rkt** / **bmbench_improved.rkt** - Boyer-Moore majority voting with configurable parallelism
- **richards.rkt** - OS simulator benchmark adapted from Martin Richards' original
- **rows1b.rkt** - Synthetic 1-billion-row data processing workload

### Shootout Benchmarks
- **spectral-norm.rkt** - Eigenvalue approximation via power iteration
- **binary-trees.rkt** - Binary tree allocation and traversal
- **nbody.rkt** - N-body gravitational simulation
- **fannkuch-redux.rkt** - Indexed access to permutations
- **mandelbrot.rkt** - Mandelbrot set fractal generation
- **chameneos.rkt** - Thread coordination benchmark

### External Suites
- **NAS Parallel Benchmarks** - EP, MG, FT, and other kernels (requires external compilation)

## Development Status

This is an active development project. Current phase focus:
- ✅ Phase 1-2: Core infrastructure and logging (complete)
- ✅ Phase 3: Shootout benchmarks integration (6 benchmarks complete)
- ✅ Phase 8: Visualization and plotting tools (complete)
- 🚧 Phase 4: NAS benchmarks (runner infrastructure complete, source vendoring pending)
- 🚧 Phase 6: Unified CLI orchestration (in progress)
- ⏳ Phase 5: MPL benchmarks (planned)
- ⏳ Phase 7: CI integration (planned)

See [PLAN.md](PLAN.md) for detailed roadmap and progress tracking.

## Requirements

- Racket 8.0 or later
- For plotting: `plot` library (included in standard Racket distribution)
- For NAS benchmarks: Fortran compiler (optional, for building NAS kernels)

## Contributing

Contributions are welcome! When adding new benchmarks:
1. Follow the established CLI patterns (see `benchmarks/common/cli.rkt`)
2. Emit structured logs via `benchmarks/common/logging.rkt`
3. Provide both sequential and parallel variants where applicable
4. Add RackUnit tests to `tests/` validating correctness
5. Document usage in `BENCHMARKS.md`

See [AGENTS.md](AGENTS.md) for detailed contribution guidelines.

## License

See individual benchmark files for licensing information. Original benchmarks from external sources retain their original licenses.
