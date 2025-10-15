# Benchmark Harness Guide

This repository currently provides a small set of Racket benchmarks with a shared command-line interface and S-expression logging. Additional suites (shootout, NAS, MPL) will plug into the same structure in later stages.

## Layout

```
benchmarks/
  common/
    cli.rkt       ; shared option parsers
    logging.rkt   ; S-expression log writer
    run.rkt       ; GC-aware timing + verification helper
  racket/
    bmbench.rkt           ; original Boyer–Moore benchmark
    bmbench_improved.rkt  ; chunked futures variant
    richards.rkt          ; futures-enabled Richards benchmark
    rows1b.rkt            ; 1B rows synthetic workload
  shootout/
    README.md
    spectral-norm.rkt     ; spectral norm with thread-based parallelism
    binary-trees.rkt      ; binary tree checksum with threads
    nbody.rkt             ; n-body gravitational simulation
    fannkuch-redux.rkt    ; fannkuch-redux permutation benchmark
    mandelbrot.rkt        ; mandelbrot set fractal generation
    chameneos.rkt         ; chameneos-redux thread coordination benchmark
    fasta.rkt             ; FASTA sequence generation benchmark
    regex-dna.rkt         ; Regex DNA pattern matching benchmark
    k-nucleotide.rkt      ; K-nucleotide frequency analysis benchmark
  nas/
    README.md             ; instructions + runner for NAS binaries
    common/               ; shared problem-class metadata
    ep.rkt                ; Embarrassingly Parallel kernel (seq + parallel)
  tools/
    analysis.rkt          ; shared log aggregation helpers
    summarize-results.rkt ; S-expression log aggregator
    plot-results.rkt      ; PNG plotting utility powered by `plot`
    visualize.rkt         ; interactive HTML dashboard generator
```

## Running the Racket Benchmarks

Each benchmark accepts a consistent set of CLI switches and will emit one `(benchmark ...)` S-expression per timed run. Examples:

```bash
# Baseline Boyer–Moore with custom sizes and workers
racket benchmarks/racket/bmbench.rkt \
  --sizes 20000,80000 \
  --workers 2,4 \
  --target-work 400000 \
  --probability 0.7 \
  --log logs/bmbench.sexp

# Improved Boyer–Moore with manual chunk tuning
racket benchmarks/racket/bmbench_improved.rkt \
  --sizes 20000 \
  --threshold 30000 \
  --chunk-size 50000 \
  --chunk-multiplier 3 \
  --log logs/bmbench-improved.sexp

# Richards benchmark with parallel sweep
racket benchmarks/racket/richards.rkt \
  --iterations 10 \
  --workers 1,4,8 \
  --log logs/richards.sexp

# Generated “1B rows” challenge benchmark
racket benchmarks/racket/rows1b.rkt \
  --rows 5000000 \
  --workers 8 \
  --chunk-size 500000 \
  --repeat 3 \
  --log logs/rows1b.sexp

# Spectral norm (shootout) benchmark
racket benchmarks/shootout/spectral-norm.rkt \
  --n 2000 \
  --iterations 15 \
  --workers 8 \
  --repeat 5 \
  --log logs/spectral-norm.sexp

# Binary trees (shootout) benchmark
racket benchmarks/shootout/binary-trees.rkt \
  --max-depth 18 \
  --workers 8 \
  --repeat 3 \
  --log logs/binary-trees.sexp

# N-body simulation (shootout) benchmark
racket benchmarks/shootout/nbody.rkt \
  --n 5000000 \
  --workers 8 \
  --repeat 3 \
  --log logs/nbody.sexp

# Fannkuch-redux (shootout) benchmark
racket benchmarks/shootout/fannkuch-redux.rkt \
  --n 10 \
  --workers 1 \
  --repeat 3 \
  --log logs/fannkuch-redux.sexp

# Mandelbrot set (shootout) benchmark
racket benchmarks/shootout/mandelbrot.rkt \
  --n 1000 \
  --workers 8 \
  --repeat 3 \
  --log logs/mandelbrot.sexp

# Chameneos-redux (shootout) benchmark
racket benchmarks/shootout/chameneos.rkt \
  --n 10000 \
  --repeat 3 \
  --log logs/chameneos.sexp

# FASTA (shootout) benchmark
racket benchmarks/shootout/fasta.rkt \
  --n 150000 \
  --workers 4 \
  --repeat 3 \
  --log logs/fasta.sexp

# Regex DNA (shootout) benchmark
racket benchmarks/shootout/regex-dna.rkt \
  --n 150000 \
  --workers 4 \
  --repeat 3 \
  --log logs/regex-dna.sexp

# K-Nucleotide (shootout) benchmark
racket benchmarks/shootout/k-nucleotide.rkt \
  --n 150000 \
  --workers 4 \
  --repeat 3 \
  --log logs/k-nucleotide.sexp
```

Key switches (all optional):

- `--sizes`: Comma-separated vector lengths (Boyer–Moore benchmarks).
- `--workers`: Worker counts to evaluate in the parallel sweeps (bench-specific meaning documented above).
- `--target-work`: Aggregate work target to normalize iteration counts (Boyer–Moore).
- `--probability`, `--kinds`, `--threshold`, `--chunk-size`, `--chunk-multiplier`: Workload tuning for Boyer–Moore variants.
- `--rows`, `--chunk-size`, `--repeat`: Generation size and scheduling parameters for the “1B rows” benchmark.
- `--n`, `--max-depth`, `--iterations`, `--workers`, `--repeat`: Controls for the shootout workloads (spectral norm, binary trees, n-body, fannkuch-redux).
- `--iterations`: Number of full Richards runs per benchmark invocation.
- `--log`: Write S-expression records to a file in addition to stdout.

## Log Format

Each run produces an S-expression of the form:

```
(benchmark
  (name bmbench)
  (variant parallel)
  (iteration 1)
  (repeat 1)
  (metrics (cpu-ms 4) (real-ms 4) (gc-ms 0))
  (params (size 1000) (repeats 1) (majority 7) ...)
  (metadata (timestamp 1758661801) (racket-version "8.18.0.16") ...)
  (status ok))
```

The `params` field differs per benchmark but always lists key/value pairs that describe the run configuration. These records are designed to be easy to parse via `read`.

## Summarizing Results

The summarizer consumes one or more `.sexp` log files and prints aggregate statistics (count, mean/σ/min/max real time, mean CPU time) grouped by benchmark name and variant.

```bash
racket benchmarks/tools/summarize-results.rkt logs/bmbench.sexp logs/richards.sexp
```

Sample output:

```
name variant count real-mean(ms) real-stddev real-min real-max cpu-mean(ms)
bmbench sequential 1.000 3.000 0.000 3.000 3.000 3.000
bmbench parallel   1.000 4.000 0.000 4.000 4.000 4.000
```

Pass `-` to read from stdin if you are piping results.

## Plotting Results

Use the plotting utility to turn aggregated statistics into a PNG bar chart. Variants are grouped per benchmark name, and you can toggle between real or CPU means.

```bash
racket benchmarks/tools/plot-results.rkt \
  --input logs/bmbench.sexp \
  --input logs/binary-trees.sexp \
  --metric real \
  --title "Benchmark (real ms)" \
  --output plots/benchmark-real.png
```

Metrics supported: `real` (default) or `cpu`. The script computes means across all runs and generates a grouped bar chart using the `plot` library.

## External Suites

### NAS Parallel Benchmarks

The NAS suite provides Racket implementations of selected NPB kernels:

```bash
# EP (Embarrassingly Parallel) benchmark
racket benchmarks/nas/ep.rkt \
  --class A \
  --workers 8 \
  --repeat 3 \
  --log logs/nas-ep.sexp

# IS (Integer Sort) benchmark
racket benchmarks/nas/is.rkt \
  --class W \
  --workers 4 \
  --repeat 3 \
  --log logs/nas-is.sexp

# CG (Conjugate Gradient) benchmark
racket benchmarks/nas/cg.rkt \
  --class S \
  --workers 4 \
  --repeat 3 \
  --log logs/nas-cg.sexp
```

Each benchmark supports problem classes S (small), W (workload), A, B, and C (increasing computational intensity). Use `--class` to select the problem size, `--workers` for parallel execution, and `--repeat` to run multiple iterations for statistical averaging.

## Running Multiple Benchmarks

Use the unified suite runner to execute multiple benchmarks with a single command:

```bash
# Run all shootout benchmarks with default parameters
racket benchmarks/run-suite.rkt --suite shootout

# Run all benchmarks with quick smoke test configuration
racket benchmarks/run-suite.rkt --suite all --config benchmarks/config/quick.sexp

# Run specific suites
racket benchmarks/run-suite.rkt --suite racket --suite shootout

# Use custom log directory
racket benchmarks/run-suite.rkt --suite shootout --log-dir my-logs
```

### Configuration Files

Predefined configuration files control problem sizes and repetitions:

- `benchmarks/config/quick.sexp` - Fast smoke tests (minimal sizes, 1 repetition)
- `benchmarks/config/standard.sexp` - Moderate workloads (typical benchmarking)
- `benchmarks/config/stress.sexp` - Large problem sizes (comprehensive evaluation)

Configuration files use S-expression format with override specifications per benchmark.

## Interactive Visualization

The visualization tool runs benchmarks and generates an interactive HTML dashboard for analyzing results:

```bash
# Run all benchmarks and generate visualization
racket benchmarks/tools/visualize.rkt --suite all --config benchmarks/config/quick.sexp

# Run specific suite with custom output
racket benchmarks/tools/visualize.rkt \
  --suite shootout \
  --config benchmarks/config/standard.sexp \
  --output results.html \
  --title "Shootout Benchmark Results"

# Generate visualization from existing logs without re-running benchmarks
racket benchmarks/tools/visualize.rkt --no-run --log-dir logs --output results.html
```

Key features:
- **Interactive Charts**: Switch between bar, line, and radar charts
- **Multiple Metrics**: View real time, CPU time, min/max values
- **Linear/Log Scale**: Toggle between linear and logarithmic scales
- **Summary Statistics**: Quick overview of total benchmarks, variants, and average times
- **Detailed Table**: Complete data with all metrics and standard deviations

Options:
- `--suite`, `-s`: Suite to run (racket, shootout, nas, mpl, or all)
- `--config`, `-c`: Configuration file (quick.sexp, standard.sexp, stress.sexp)
- `--log-dir`, `-l`: Directory for log files (default: logs)
- `--output`, `-o`: Output HTML file (default: benchmark-results.html)
- `--title`, `-t`: Dashboard title
- `--no-run`: Generate visualization from existing logs only

## Next Steps

- Add additional shootout workloads (fasta, regex-dna, k-nucleotide, etc.) following the shared harness.
- Integrate MPL benchmarks with shared logging conventions.
- Extend the summarizer/plot tools to compute speedups once multiple variants per workload are available.
