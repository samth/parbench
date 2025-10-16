# AI Agent Guidelines for Parallel Benchmark Suite

This document provides guidance for AI coding assistants (Claude, GitHub Copilot, etc.) working on this Racket parallel benchmarking repository.

## Repository Overview

This is a **comprehensive parallel benchmarking suite** for Racket, containing 19+ benchmarks across 4 categories:
- **Racket benchmarks** (3): bmbench, bmbench_improved, richards, rows1b
- **Shootout benchmarks** (9): spectral-norm, binary-trees, nbody, fannkuch-redux, mandelbrot, chameneos, fasta, regex-dna, k-nucleotide
- **NAS benchmarks** (3): ep, is, cg
- **MPL benchmarks** (7): histogram, integer-sort, bfs, mis, msf, suffix-array, convex-hull

## Architecture & Key Modules

### Core Infrastructure (`benchmarks/common/`)
- **`cli.rkt`**: Shared command-line argument parsing (--log, --workers, --repeat, etc.)
- **`logging.rkt`**: S-expression log writer for benchmark results
- **`run.rkt`**: GC-aware timing and verification helpers
- **`parallel.rkt`**: Parallel loop macros and utilities

### Result Format
All benchmarks emit S-expressions with this structure:
```scheme
(benchmark
  (name <benchmark-name>)
  (variant sequential|parallel)
  (iteration <n>)
  (repeat <n>)
  (metrics (cpu-ms <ms>) (real-ms <ms>) (gc-ms <ms>))
  (params (<key> <value>) ...)
  (metadata (timestamp <unix-time>) (racket-version <version>) ...)
  (status ok|error))
```

### Analysis Tools (`benchmarks/tools/`)
- **`analysis.rkt`**: Log aggregation and statistical helpers
- **`summarize-results.rkt`**: CLI tool for computing means/stddev from logs
- **`plot-results.rkt`**: PNG generation using `plot` library
- **`visualize.rkt`**: Interactive HTML dashboard generator

### Suite Runner (`benchmarks/run-suite.rkt`)
Unified CLI for running multiple benchmarks:
```bash
racket benchmarks/run-suite.rkt --suite racket|shootout|nas|mpl|all --config config/quick.sexp
```

## Development Guidelines

### Adding New Benchmarks

When adding a new benchmark, follow this checklist:

1. **Choose the appropriate directory:**
   - `benchmarks/racket/` - Native Racket algorithms
   - `benchmarks/shootout/` - Computer Language Benchmarks Game ports
   - `benchmarks/nas/` - NAS Parallel Benchmark implementations
   - `benchmarks/mpl/` - MPL benchmark re-implementations

2. **Required structure:**
   ```racket
   #lang racket
   (require "../common/cli.rkt"
            "../common/logging.rkt"
            "../common/run.rkt")

   ;; Sequential implementation
   (define (benchmark-sequential params)
     ...)

   ;; Parallel implementation
   (define (benchmark-parallel params workers)
     ...)

   ;; CLI entry point
   (module+ main
     (define args (parse-args ...))
     (log-benchmark ...))
   ```

3. **Add tests** in `tests/`:
   - Verify correctness (sequential vs parallel outputs match)
   - Add smoke tests with small problem sizes
   - Use RackUnit framework

4. **Update documentation:**
   - Add usage examples to `BENCHMARKS.md`
   - Update suite-specific README (e.g., `benchmarks/nas/README.md`)
   - List new benchmark in main `README.md`

5. **Integrate with suite runner:**
   - Add entry to `benchmarks/run-suite.rkt` in appropriate suite
   - Provide default parameters in config files (`benchmarks/config/*.sexp`)

### Coding Standards

- **Use shared infrastructure:** Import from `benchmarks/common/` rather than duplicating CLI/logging code
- **Provide both variants:** Sequential and parallel implementations for comparison
- **Parameter validation:** Use `cli.rkt` helpers to validate arguments
- **Deterministic where possible:** Use fixed random seeds for reproducibility
- **Verification:** Include correctness checks (checksums, invariants)
- **Documentation strings:** Add docstrings for key functions
- **Type annotations (optional):** Consider Typed Racket for performance-critical code

### Performance Considerations

- **GC awareness:** Use `collect-garbage` before timing critical sections
- **Warm-up runs:** Consider discarding first iteration for JIT warmup
- **Granularity tuning:** Expose `--chunk-size` or `--threshold` parameters
- **Worker counts:** Test scaling from 1 to N workers (N = core count)
- **Memory layout:** Use vectors over lists for large datasets
- **Avoid allocation in hot paths:** Pre-allocate result buffers where possible

### Testing Strategy

1. **Correctness tests:** Sequential vs parallel output equivalence
2. **Smoke tests:** Small problem sizes (< 1s runtime)
3. **Verification tests:** Known outputs or checksums (especially NAS/MPL)
4. **Scaling tests:** Verify speedup with increased workers
5. **Regression tests:** Compare against historical performance data

Run tests with:
```bash
raco test tests/                    # All tests
raco test tests/nas/ep-test.rkt     # Specific test
```

## Common Patterns

### CLI Argument Parsing
```racket
(define (parse-benchmark-args)
  (command-line
   #:program "benchmark-name"
   #:once-each
   [("--n") n "Problem size" (set! param-n (string->number n))]
   [("--workers") w "Worker count" (set! workers (string->number w))]
   [("--repeat") r "Repetitions" (set! repeats (string->number r))]
   [("--log") file "Log file" (set! log-file file)]))
```

### Timing and Logging
```racket
(for ([i (in-range repeats)])
  (collect-garbage)
  (define-values (result cpu-ms real-ms gc-ms)
    (time-apply benchmark-fn args))
  (log-benchmark-result
   #:name "benchmark-name"
   #:variant (if (= workers 1) "sequential" "parallel")
   #:metrics (list (cons 'cpu-ms cpu-ms)
                   (cons 'real-ms real-ms)
                   (cons 'gc-ms gc-ms))
   #:params (list (cons 'n n) (cons 'workers workers))
   #:log-file log-file))
```

### Parallel Loops
```racket
;; Using futures
(define chunks (chunk-work data chunk-size))
(define results
  (map touch
       (map (lambda (chunk) (future (lambda () (process chunk))))
            chunks)))

;; Using for/parallel (custom macro)
(for/parallel ([item (in-list items)] #:workers workers)
  (expensive-computation item))
```

## Benchmark-Specific Guidance

### NAS Benchmarks
- **Problem classes:** S (small), W (workstation), A/B/C (increasing)
- **Verification:** Compare against known checksums from NPB specification
- **Implementation notes:**
  - EP: Partition random number generation across workers
  - IS: Use parallel bucket counting then merge
  - CG: Parallelize sparse matrix-vector multiply

### MPL Benchmarks
- **Graph algorithms:** Support both synthetic generation and file input
- **Verification:** Sequential and parallel must produce equivalent results
- **Randomization:** Expose `--seed` parameter for reproducibility
- **Implementation strategy:**
  - Use standard parallel patterns (map, reduce, scan)
  - Provide sequential baseline for correctness validation
  - Consider granularity thresholds (switch to sequential for small subproblems)

### Shootout Benchmarks
- **Compatibility:** Maintain behavioral equivalence with reference implementations
- **Parameter ranges:** Document typical problem sizes from benchmark game
- **Output:** Some benchmarks produce output (fasta, regex-dna) - handle appropriately

## Troubleshooting

### Common Issues

1. **"No such module" errors:**
   - Ensure you're running from repository root
   - Check relative paths in `require` statements
   - Verify `info.rkt` if creating new packages

2. **Performance doesn't scale:**
   - Check chunk size (too small = overhead, too large = load imbalance)
   - Profile with `racket -l errortrace -t benchmark.rkt`
   - Verify futures aren't being blocked (use `future-visualizer`)

3. **Tests fail with "results differ":**
   - Check for non-determinism (random seeds, hash table ordering)
   - Verify floating-point tolerance in comparisons
   - Ensure parallel reduction is order-independent

4. **Visualization produces empty output:**
   - Confirm log files exist and contain valid S-expressions
   - Check that all required fields are present in log entries
   - Verify `plot` library is installed

## File Organization

```
/code/
├── README.md                 # Main project overview
├── BENCHMARKS.md            # Detailed usage guide
├── PLAN.md                  # Development roadmap
├── AGENTS.md                # Contributor guidelines
├── CLAUDE.md                # This file
├── .gitignore
├── info.rkt
├── benchmarks/
│   ├── common/              # Shared infrastructure
│   ├── racket/              # Native Racket benchmarks
│   ├── shootout/            # Shootout ports
│   ├── nas/                 # NAS implementations
│   ├── mpl/                 # MPL re-implementations
│   ├── tools/               # Analysis and visualization
│   ├── config/              # Configuration files
│   └── run-suite.rkt        # Unified CLI
└── tests/                   # RackUnit test suite
    ├── racket/
    ├── shootout/
    ├── nas/
    └── mpl/
```

## Important Notes

### What NOT to Do
- ❌ Don't modify `benchmarks/common/` without updating all benchmarks
- ❌ Don't commit log files (`logs/*.sexp`) or generated visualizations
- ❌ Don't skip verification tests - correctness is paramount
- ❌ Don't use platform-specific APIs (keep cross-platform)
- ❌ Don't hardcode absolute paths
- ❌ Don't commit benchmarks with known correctness issues

### When to Ask for Review
- Adding new benchmark categories beyond racket/shootout/nas/mpl
- Modifying shared infrastructure in `benchmarks/common/`
- Changing log format (breaks existing analysis tools)
- Adding external dependencies beyond standard Racket distribution
- Performance regressions > 20% on existing benchmarks

## Quick Reference Commands

```bash
# Run single benchmark
racket benchmarks/nas/ep.rkt --class S --workers 4 --repeat 3

# Run suite with config
racket benchmarks/run-suite.rkt --suite all --config config/quick.sexp

# Summarize results
racket benchmarks/tools/summarize-results.rkt logs/*.sexp

# Generate PNG plot
racket benchmarks/tools/plot-results.rkt --input logs/*.sexp --output plot.png

# Generate interactive HTML dashboard
racket benchmarks/tools/visualize.rkt --suite all --config config/quick.sexp

# Run tests
raco test tests/

# Check benchmark without running
racket -c benchmarks/nas/ep.rkt
```

## Benchmark Counts by Suite

- **Racket:** 4 benchmarks (bmbench, bmbench_improved, richards, rows1b)
- **Shootout:** 9 benchmarks (spectral-norm, binary-trees, nbody, fannkuch-redux, mandelbrot, chameneos, fasta, regex-dna, k-nucleotide)
- **NAS:** 3 benchmarks (ep, is, cg) + 2 planned (mg, ft)
- **MPL:** 7 benchmarks (histogram, integer-sort, bfs, mis, msf, suffix-array, convex-hull)
- **Total:** 23 implemented, 2 planned

## Version History

- **2025-10-16:** Comprehensive suite with all major benchmarks complete
- **2025-10-15:** Integrated NAS and MPL benchmarks, expanded shootout suite
- **2025-10-07:** Added visualization dashboard
- **2025-10-02:** Core infrastructure and initial benchmarks

## References

- [Racket Documentation](https://docs.racket-lang.org/)
- [NAS Parallel Benchmarks](https://www.nas.nasa.gov/software/npb.html)
- [MPL Parallel ML Benchmarks](https://github.com/MPLLang/parallel-ml-bench)
- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- [PBBS Benchmark Suite](https://cmuparlay.github.io/pbbsbench/)

---

**Remember:** When in doubt, consult existing benchmarks as examples. The codebase is designed for consistency and extensibility - follow established patterns!
