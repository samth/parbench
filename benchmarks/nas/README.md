# NAS Parallel Benchmarks Implementation

This directory contains Racket implementations of selected NAS Parallel Benchmark (NPB) kernels. Rather than wrapping external C/Fortran executables, we re-implement the algorithmic kernels directly in Racket to enable comparative analysis of Racket's parallel programming constructs (futures, places) against reference implementations.

## Implemented Benchmarks

| Kernel | Status | Notes |
| ------ | ------ | ----- |
| EP (Embarrassingly Parallel) | 🚧 In progress | Gaussian deviates via Box–Muller, strong scaling focus |
| IS (Integer Sort) | ⏳ Planned | Bucket + prefix communication; will reuse histogram utilities |
| CG (Conjugate Gradient) | ⏳ Planned | Sparse matrix-vector operations with verification norm |
| MG (Multi-Grid) | ⏳ Planned | V-cycle over 3D grid; candidate for places |
| FT (Fourier Transform) | ⏳ Planned | 3D FFT (Stockham) with checksum comparison |

Each benchmark will provide:
- Sequential and parallel variants using Racket's futures and/or places
- CLI interface with problem class selection (S, W, A, B, C)
- Verification checksums to validate correctness
- Integration with the shared logging infrastructure

## Roadmap & Directory Layout

```
benchmarks/nas/
  common/
    classes.rkt      ; problem-class parameters (total points, random seeds, etc.)
    verification.rkt ; shared checksum helpers
  ep.rkt             ; EP benchmark driver (seq/parallel)
  is.rkt             ; Integer Sort driver
  ...
```

### Phase 4 Checklist

- [ ] Implement EP kernel with configurable class parameters
- [ ] Provide futures-based parallel variant and deterministic verification
- [ ] Add smoke-sized tests mirroring class S workloads
- [ ] Update `benchmarks/run-suite.rkt` to expose NAS suite runs
- [ ] Document invocation examples per kernel

## Planned Kernels

1. **EP (Embarrassingly Parallel)**: Generate independent Gaussian random deviates
   - Classes map to total pairs: S=2¹⁴, W=2¹⁷, A=2²⁴, B=2²⁶, C=2²⁸ (per NPB 3.4 spec)
   - Verification: compare partial sums `sx`, `sy` and counts per annulus
2. **IS (Integer Sort)**: Integer bucket sort with all-to-all communication pattern
3. **CG (Conjugate Gradient)**: Sparse linear system solver
4. **MG (Multi-Grid)**: Multi-grid method for Poisson equation
5. **FT (Fourier Transform)**: 3D FFT-based PDE solver

## Usage Example

Pending EP completion the command-line interface will resemble:

```bash
# Example (coming soon once ep.rkt lands)
racket benchmarks/nas/ep.rkt \
  --class A \
  --workers 8 \
  --repeat 3 \
  --log logs/nas-ep.sexp
```

## References

- [NAS Parallel Benchmarks Specification](https://www.nas.nasa.gov/software/npb.html)
- Problem classes define computational scale (S < W < A < B < C < D)
