# Racket Shootout Benchmarks

This directory hosts re-implementations of classic computer language shootout workloads using the shared benchmarking harness (CLI, logging, and summarizer). Benchmarks included so far:

- `spectral-norm.rkt`: Computes the spectral norm of an implicitly defined matrix using both sequential and parallel (futures) implementations.

Each benchmark supports the standard `--log` flag for S-expression output and accepts task-specific tuning parameters. Additional shootout programs (binary trees, n-body, etc.) can be integrated by following the same pattern.
