# AI Agent Guidelines for Parallel Benchmark Suite

This document provides guidance for AI coding assistants (Claude, GitHub Copilot, etc.) working on this Racket parallel benchmarking repository.

## Repository Overview

This is a **comprehensive parallel benchmarking suite** for Racket, containing benchmarks across 3 categories:
- **Racket benchmarks** (3): bmbench, richards, rows1b
- **Shootout benchmarks** (6): binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna
- **MPL benchmarks** (27): histogram, integer-sort, bfs, mis, msf, suffix-array, convex-hull, plus 20 additional benchmarks

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
racket benchmarks/run-suite.rkt --suite racket|shootout|mpl|all --config config/quick.sexp
```

## Development Guidelines

### Adding New Benchmarks

When adding a new benchmark, follow this checklist:

1. **Choose the appropriate directory:**
   - `benchmarks/racket/` - Native Racket algorithms
   - `benchmarks/shootout/` - Computer Language Benchmarks Game ports
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

### Submodule Usage

**Always use `main` and `test` submodules correctly:**

- **`(module+ main ...)`**: Code that should only run when the file is executed directly (not when required). Use for CLI entry points, benchmark runners, and any code with side effects.
  ```racket
  (module+ main
    (define args (current-command-line-arguments))
    (run-benchmark args))
  ```

- **`(module+ test ...)`**: Code that should only run during testing with `raco test`. Use for unit tests and test setup.
  ```racket
  (module+ test
    (require rackunit)
    (check-equal? (fib 10) 55))
  ```

- **Top-level code**: Only define functions, structs, and constants at the top level. Never put side-effecting code (prints, file I/O, benchmarks) at the top level—it will run on every `require`.

- **Why this matters**: `raco test .` loads all `.rkt` files to check for test submodules. Files with top-level side effects or syntax errors will break the test suite. Files without a `test` submodule are simply skipped.

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
3. **Verification tests:** Known outputs or checksums (especially MPL)
4. **Scaling tests:** Verify speedup with increased workers
5. **Regression tests:** Compare against historical performance data
6. **CLI integration tests:** Test `./bench` output using recspecs expect tests

Run tests with:
```bash
raco test tests/                    # All tests
raco test tests/bmbench-test.rkt    # Specific test
```

### Quick Testing During Development

**Always run quick smoke tests after making changes** to verify benchmarks still work:

```bash
# Very fast smoke test (tiny problem sizes, 1 iteration, few cores)
./bench --work 0.001 --iterations 1 --cores 1,2 fib histogram

# Quick mode (3 iterations, small sizes)
./bench --quick fib

# Test specific suite with minimal work
./bench --work 0.01 --iterations 1 shootout
```

**Key flags for quick testing:**
- `--work 0.001` — Scale problem sizes to 0.1% of normal (very fast)
- `--work 0.01` — Scale to 1% of normal (fast)
- `--iterations 1` — Single iteration per benchmark
- `--cores 1,2` — Test with 1 and 2 cores only
- `--quick` — Built-in quick mode (3 iterations)

**When to use which:**
- After code changes: `./bench --work 0.001 --iterations 1 --cores 1 <benchmark>`
- Before committing: `./bench --quick <affected-benchmarks>`
- Full validation: `./bench <suite>` (with default settings)

### Critical: Test-Benchmark Alignment

**Before writing or modifying tests, ALWAYS read the benchmark file first** to verify:

1. **Data types must match exactly:**
   - `fxvector` for integer-sort (NOT `vector`)
   - `flvector` for mcss and other floating-point benchmarks (NOT `vector`)
   - `bytes` for bignum-add (NOT `vector`)
   - `(vector x y)` for convex-hull points (NOT `(cons x y)` pairs)
   - `graph` structs for bfs/centrality (use `make-graph` or `generate-random-graph`)

2. **Function signatures must match:**
   - Check the `provide` statement to see exported functions
   - Count parameters carefully (e.g., `bfs-sequential` takes 3 args: graph, source, workers)
   - Check return types (single value vs `values` for multiple returns)

3. **CLI flags must exist:**
   - Read the `command-line` section in the benchmark's `main` module
   - Common flags: `--n`, `--workers`, `--repeat`, `--log`
   - Each benchmark may have unique flags (check before assuming)

4. **Test only existing benchmarks:**
   - If a test references a file that doesn't exist, delete the test
   - Don't create tests for planned/future benchmarks

5. **Keep test sizes small:**
   - Use small problem sizes to avoid timeouts (< 5s per test)
   - Reduce worker counts for parallel tests (2-4 workers max)

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
   - Ensure the `plot` collection is required and loaded (it's part of the standard distribution)

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
│   ├── mpl/                 # MPL re-implementations
│   ├── tools/               # Analysis and visualization
│   ├── config/              # Configuration files
│   └── run-suite.rkt        # Unified CLI
└── tests/                   # RackUnit test suite
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
- Adding new benchmark categories beyond racket/shootout/mpl
- Modifying shared infrastructure in `benchmarks/common/`
- Changing log format (breaks existing analysis tools)
- Adding external dependencies beyond standard Racket distribution
- Performance regressions > 20% on existing benchmarks

## Quick Reference Commands

```bash
# Run benchmarks (or use: raco parbench after installing package)
./bench fib --quick
./bench mpl --cores 1,4,8
./bench --help

# Run single benchmark directly
racket benchmarks/mpl/fib.rkt --n 40 --workers 4 --repeat 3

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
racket -c benchmarks/mpl/fib.rkt
```

## Benchmark Counts by Suite

- **Racket:** 3 benchmarks (bmbench, richards, rows1b)
- **Shootout:** 6 benchmarks (binary-trees, spectral-norm, fannkuch-redux, mandelbrot, k-nucleotide, regex-dna)
- **MPL:** 27 benchmarks (graph algorithms, sorting, text processing, numeric computations)
- **Total:** 36 active benchmarks

## Version History

- **2025-10-16:** Comprehensive suite with all major benchmarks complete
- **2025-10-15:** Integrated MPL benchmarks, expanded shootout suite
- **2025-10-07:** Added visualization dashboard
- **2025-10-02:** Core infrastructure and initial benchmarks

## References

- [Racket Documentation](https://docs.racket-lang.org/)
- [MPL Parallel ML Benchmarks](https://github.com/MPLLang/parallel-ml-bench)
- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- [PBBS Benchmark Suite](https://cmuparlay.github.io/pbbsbench/)

---

**Remember:** When in doubt, consult existing benchmarks as examples. The codebase is designed for consistency and extensibility - follow established patterns!
