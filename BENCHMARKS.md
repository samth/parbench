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
  shootout/              ; placeholder for Racket shootout workloads (Stage 3)
  nas/                   ; placeholder for NAS kernels (Stage 4)
  mpl/                   ; placeholder for MPL benchmarks (Stage 5)
  tools/
    summarize-results.rkt ; S-expression log aggregator
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

```

Key switches (all optional):

- `--sizes`: Comma-separated vector lengths (Boyer–Moore benchmarks).
- `--workers`: Worker counts to evaluate in the parallel sweeps.
- `--target-work`: Aggregate work target to normalize iteration counts.
- `--probability`, `--kinds`, `--threshold`, `--chunk-size`, `--chunk-multiplier`: Workload-tuning parameters for the Boyer–Moore variants.
- `--rows`, `--chunk-size`, `--repeat`: Generation size and scheduling parameters for the “1B rows” benchmark.
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

## Next Steps

- Add additional shootout workloads (binary trees, n-body, etc.) following the `spectral-norm` pattern.
- Integrate MPL benchmarks with shared logging conventions.
- Extend the summarizer to compute speedups once multiple variants per workload are available.
