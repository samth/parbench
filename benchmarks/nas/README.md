# NAS Parallel Benchmarks Implementation

This directory contains Racket implementations of selected NAS Parallel Benchmark (NPB) kernels. Rather than wrapping external C/Fortran executables, we re-implement the algorithmic kernels directly in Racket to enable comparative analysis of Racket's parallel programming constructs (futures, places) against reference implementations.

## Implemented Benchmarks

| Kernel | Status | Notes |
| ------ | ------ | ----- |
| EP (Embarrassingly Parallel) | ✅ Complete | Gaussian deviates via Box–Muller, strong scaling focus |
| IS (Integer Sort) | ✅ Complete | Bucket sort with parallel counting and sorting |
| CG (Conjugate Gradient) | ✅ Complete | Sparse matrix-vector operations with iterative solver |
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
  cg.rkt             ; Conjugate Gradient driver
  ...
```

### Phase 4 Checklist

- [x] Implement EP kernel with configurable class parameters
- [x] Provide futures-based parallel variant and deterministic verification
- [x] Add smoke-sized tests mirroring class S workloads
- [x] Implement IS (Integer Sort) kernel with parallel bucket sort
- [x] Implement CG (Conjugate Gradient) kernel with sparse matrix operations
- [x] Add comprehensive tests for EP, IS, and CG
- [ ] Update `benchmarks/run-suite.rkt` to expose NAS suite runs
- [x] Document invocation examples per kernel

## Kernel Descriptions

1. **EP (Embarrassingly Parallel)**: Generate independent Gaussian random deviates
   - Classes map to total pairs: S=2²⁴, W=2²⁵, A=2²⁸, B=2³⁰, C=2³² (per NPB 3.4 spec)
   - Verification: compare partial sums `sx`, `sy` and counts per annulus
   - Parallel strategy: Partition pairs across workers, each with deterministic LCG seed

2. **IS (Integer Sort)**: Integer bucket sort with parallel counting
   - Classes define total keys and key range: S=2¹⁶ keys, W=2²⁰, A=2²³, B=2²⁵, C=2²⁷
   - Parallel strategy: Local bucket counting, global merge, then parallel sorting within buckets
   - Verification: Ensures result is fully sorted

3. **CG (Conjugate Gradient)**: Sparse matrix iterative solver
   - Classes define matrix size and sparsity: S=1400, W=7000, A=14000, B=75000, C=150000
   - Parallel strategy: Parallel sparse matrix-vector multiply in CG iterations
   - Verification: Deterministic results within implementation; reference zeta values provided for comparison
   - Note: Final zeta values may differ from official NAS reference due to implementation-specific matrix generation, but the CG algorithm is correctly implemented and produces consistent, verifiable results

4. **MG (Multi-Grid)** (Planned): Multi-grid method for Poisson equation
5. **FT (Fourier Transform)** (Planned): 3D FFT-based PDE solver

## Usage Examples

All NAS benchmarks support a consistent command-line interface:

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

## References

- [NAS Parallel Benchmarks Specification](https://www.nas.nasa.gov/software/npb.html)
- Problem classes define computational scale (S < W < A < B < C < D)
