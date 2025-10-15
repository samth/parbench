# Racket Shootout Benchmarks

This directory hosts re-implementations of classic computer language shootout workloads using the shared benchmarking harness (CLI, logging, summarizer/plot tools).

## Included Benchmarks

- `spectral-norm.rkt`: Spectral norm of an implicit matrix with sequential and thread-parallel variants.
- `binary-trees.rkt`: Binary tree checksum benchmark with sequential and thread-parallel execution.
- `nbody.rkt`: N-body gravitational simulation with sequential and parallel force computation.
- `fannkuch-redux.rkt`: Fannkuch-redux permutation benchmark (sequential only).
- `mandelbrot.rkt`: Mandelbrot set fractal generation with sequential and parallel row computation.
- `chameneos.rkt`: Chameneos-redux thread coordination and meeting benchmark.
- `fasta.rkt`: FASTA sequence generator benchmark emitting deterministic pseudo-random DNA workloads.
- `regex-dna.rkt`: Regex DNA benchmark exercising pattern matching and substitution on the generated sequences.
- `k-nucleotide.rkt`: K-Nucleotide frequency analysis benchmark with worker-parallel hash aggregation.

Each benchmark accepts the standard `--log` flag to emit `(benchmark ...)` S-expressions and exposes workload-specific knobs (problem size, iteration count, worker threads). Reduced-size configurations suitable for smoke testing are documented in `BENCHMARKS.md`.

## Notes

Some benchmarks like fannkuch-redux are inherently sequential and don't benefit from parallelization. The parallel variant in these cases runs the same sequential algorithm to maintain API consistency.

The chameneos benchmark focuses on thread coordination and synchronization patterns rather than pure computational throughput, making it a good test of Racket's threading primitives.

The `src/` subdirectory vendors the original benchmark-game reference programs for comparison. They remain standalone scripts and are not wired into the harness.
