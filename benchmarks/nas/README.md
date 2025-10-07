# NAS Parallel Benchmarks Implementation

This directory contains Racket implementations of selected NAS Parallel Benchmark (NPB) kernels. Rather than wrapping external C/Fortran executables, we re-implement the algorithmic kernels directly in Racket to enable comparative analysis of Racket's parallel programming constructs (futures, places) against reference implementations.

## Implemented Benchmarks

_(Coming soon)_

Each benchmark will provide:
- Sequential and parallel variants using Racket's futures and/or places
- CLI interface with problem class selection (S, W, A, B, C)
- Verification checksums to validate correctness
- Integration with the shared logging infrastructure

## Planned Kernels

1. **EP (Embarrassingly Parallel)**: Generate independent Gaussian random deviates
2. **IS (Integer Sort)**: Integer bucket sort with all-to-all communication pattern
3. **CG (Conjugate Gradient)**: Sparse linear system solver
4. **MG (Multi-Grid)**: Multi-grid method for Poisson equation
5. **FT (Fourier Transform)**: 3D FFT-based PDE solver

## Usage Example

_(Coming soon)_

```bash
racket benchmarks/nas/ep.rkt \
  --class A \
  --variant parallel \
  --repeat 3 \
  --log logs/nas-ep.sexp
```

## References

- [NAS Parallel Benchmarks Specification](https://www.nas.nasa.gov/software/npb.html)
- Problem classes define computational scale (S < W < A < B < C < D)
