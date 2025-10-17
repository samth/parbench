# Sandmark Benchmark Suite Porting Plan

## Executive Summary

This document outlines a plan to port benchmarks from the **Sandmark** benchmark suite (OCaml's multicore benchmarking suite) to this Racket parallel benchmarking repository. Sandmark contains ~40+ benchmarks across multiple categories testing sequential and parallel performance of OCaml's multicore implementation.

**Repository:** https://github.com/ocaml-bench/sandmark

## Sandmark Overview

Sandmark is a comprehensive benchmark suite for the OCaml compiler, developed primarily to support the OCaml Multicore project. It includes:

- **Sequential benchmarks** - baseline single-core performance
- **Parallel/multicore benchmarks** - scalability across multiple cores
- **Measurement tools** - runtime, GC statistics, and performance profiling
- **Visualization** - interactive dashboards and plots

### Key Sandmark Categories

1. **multicore-numerical** - Numerical algorithms and simulations
2. **multicore-structures** - Concurrent data structures
3. **multicore-lockfree** - Lock-free data structure implementations
4. **multicore-grammatrix** - Gram matrix computation (ML kernel)
5. **multicore-minilight** - Monte Carlo path tracing renderer
6. **benchmarksgame** - Computer Language Benchmarks Game ports
7. **graph500seq** - Graph500 BFS and SSSP implementations
8. **numerical-analysis** - Additional numerical algorithms

## Current State of Racket Suite

### Already Implemented (23 benchmarks)

**Racket Suite (4):**
- bmbench, bmbench_improved, richards, rows1b

**Shootout Suite (9):**
- spectral-norm, binary-trees, nbody, fannkuch-redux, mandelbrot, chameneos, fasta, regex-dna, k-nucleotide

**NAS Suite (3):**
- ep (Embarrassingly Parallel), is (Integer Sort), cg (Conjugate Gradient)

**MPL Suite (7):**
- histogram, integer-sort, bfs, mis, msf, suffix-array, convex-hull

### Overlap Analysis

**Already covered by existing suites:**
- spectral-norm, nbody, mandelbrot, chameneos ✓ (in shootout)
- BFS ✓ (in MPL as graph algorithm)
- Histogram ✓ (in MPL)

**Sandmark-unique benchmarks to port:**
- Matrix multiplication, LU decomposition, Floyd-Warshall
- Game of Life, Evolutionary algorithm
- Merge sort, Quick sort
- Lock-free data structures (queues, stacks, hash tables)
- Gram matrix computation
- Minilight ray tracer
- Graph500 implementations

## Porting Strategy

### Phase 1: High-Value Numerical Benchmarks (Priority: HIGH)

These benchmarks provide essential coverage of numerical parallel algorithms not currently in the suite.

#### 1.1 Matrix Operations (NEW CATEGORY: `benchmarks/sandmark/`)

**Matrix Multiplication** (`matrix-mult.rkt`)
- **Description:** Dense matrix multiplication with parallel decomposition
- **Input:** Matrix dimensions (N×N), worker count
- **Parallelization:** Block/tile decomposition across workers
- **Validation:** Compare sequential vs parallel results
- **Estimated effort:** 2-3 days
- **Value:** Core linear algebra operation, tests memory bandwidth and cache efficiency

**LU Decomposition** (`lu-decomp.rkt`)
- **Description:** LU factorization of dense matrices
- **Input:** Matrix size N, worker count
- **Parallelization:** Parallel initialization + parallel factorization steps
- **Validation:** Verify A = LU reconstruction
- **Estimated effort:** 3-4 days
- **Value:** Important numerical kernel, tests coordination of parallel updates

**Matrix Tiling Variant** (`matrix-mult-tiled.rkt`)
- **Description:** Cache-optimized tiled matrix multiplication
- **Input:** Matrix size, tile size, worker count
- **Parallelization:** Parallel tile processing
- **Validation:** Compare with non-tiled version
- **Estimated effort:** 2 days
- **Value:** Demonstrates cache optimization importance

#### 1.2 Graph Algorithms

**Floyd-Warshall** (`floyd-warshall.rkt`)
- **Description:** All-pairs shortest path for dense graphs
- **Input:** Graph adjacency matrix, worker count
- **Parallelization:** Parallelize k-loop iterations
- **Validation:** Known shortest path values
- **Estimated effort:** 2-3 days
- **Value:** Classic dynamic programming algorithm, different pattern from BFS

**Graph500 SSSP** (`graph500-sssp.rkt`)
- **Description:** Single-Source Shortest Path on Kronecker graphs
- **Input:** Scale, edge factor, root vertex
- **Parallelization:** Parallel delta-stepping or Bellman-Ford
- **Validation:** Compare with sequential version
- **Estimated effort:** 4-5 days
- **Value:** Complements existing BFS benchmark, tests weighted graph algorithms
- **Note:** Graph500 BFS already covered by MPL suite

#### 1.3 Sorting Algorithms

**Merge Sort** (`merge-sort.rkt`)
- **Description:** Parallel divide-and-conquer merge sort
- **Input:** Array size, worker count, granularity threshold
- **Parallelization:** Recursive parallel split, sequential merge for small arrays
- **Validation:** Verify sorted output
- **Estimated effort:** 2 days
- **Value:** Classic divide-and-conquer pattern

**Quick Sort** (`quick-sort.rkt`)
- **Description:** Parallel quicksort with pivot partitioning
- **Input:** Array size, worker count, threshold
- **Parallelization:** Parallel partition, recursive parallel sort
- **Validation:** Verify sorted output
- **Estimated effort:** 2-3 days
- **Value:** Alternative sorting pattern, tests load balancing with uneven partitions

### Phase 2: Concurrent Data Structures (Priority: MEDIUM)

Create new category `benchmarks/concurrent-ds/` for lock-free and concurrent data structure benchmarks.

#### 2.1 Lock-Free Queues

**MS Queue** (`ms-queue.rkt`)
- **Description:** Michael-Scott lock-free queue
- **Operations:** Concurrent enqueue/dequeue from multiple threads
- **Metrics:** Throughput (ops/sec), latency distribution
- **Validation:** All enqueued items are dequeued exactly once
- **Estimated effort:** 3-4 days
- **Value:** Classic lock-free algorithm, tests CAS performance

**SPSC Queue** (`spsc-queue.rkt`)
- **Description:** Single-producer single-consumer lock-free queue
- **Operations:** Producer thread enqueues, consumer thread dequeues
- **Metrics:** Throughput, latency
- **Validation:** FIFO ordering preserved
- **Estimated effort:** 2 days
- **Value:** High-performance specialization, important for producer-consumer patterns

**Work-Stealing Queue** (`ws-queue.rkt`)
- **Description:** Double-ended queue for work-stealing schedulers
- **Operations:** Owner pushes/pops from one end, thieves steal from other
- **Metrics:** Steal success rate, throughput
- **Validation:** All work items processed exactly once
- **Estimated effort:** 4-5 days
- **Value:** Critical for modern parallel runtime systems

#### 2.2 Lock-Free Stacks and Other Structures

**Treiber Stack** (`treiber-stack.rkt`)
- **Description:** Lock-free LIFO stack
- **Operations:** Concurrent push/pop
- **Metrics:** Throughput, contention under high load
- **Validation:** LIFO ordering, no lost elements
- **Estimated effort:** 2-3 days
- **Value:** Simpler than queues, good CAS introduction

**Lock-Free Hash Table** (`lf-hash.rkt`)
- **Description:** Concurrent hash table with lock-free operations
- **Operations:** Insert, lookup, delete
- **Metrics:** Throughput for read-heavy, write-heavy, mixed workloads
- **Validation:** All inserted keys retrievable
- **Estimated effort:** 5-6 days
- **Value:** Complex algorithm, tests scalability on mixed operations

**Lock-Free List** (`lf-list.rkt`)
- **Description:** Lock-free linked list
- **Operations:** Insert, delete, contains
- **Metrics:** Throughput under contention
- **Validation:** Set semantics preserved
- **Estimated effort:** 4 days
- **Value:** Alternative to hash tables for sorted data

### Phase 3: Specialized Algorithms (Priority: MEDIUM-LOW)

#### 3.1 Simulation and Graphics

**Game of Life** (`game-of-life.rkt`)
- **Description:** Conway's Game of Life cellular automaton
- **Input:** Grid size, generations, worker count
- **Parallelization:** Domain decomposition with halo exchange
- **Validation:** Known patterns (gliders, blinkers)
- **Estimated effort:** 2-3 days
- **Value:** Classic stencil computation, tests neighbor communication

**Minilight Ray Tracer** (`minilight.rkt`)
- **Description:** Monte Carlo path tracing renderer
- **Input:** Scene file, samples per pixel, image size
- **Parallelization:** Parallel pixel/tile rendering
- **Validation:** Visual inspection + checksum
- **Estimated effort:** 5-7 days
- **Value:** Complex real-world application, tests floating-point and random number generation

#### 3.2 Machine Learning Kernels

**Gram Matrix** (`grammatrix.rkt`)
- **Description:** Compute gram matrix (all pairwise dot products)
- **Input:** Feature matrix (samples × features), worker count
- **Parallelization:** Parallel computation of matrix entries
- **Validation:** Compare with sequential version
- **Estimated effort:** 2-3 days
- **Value:** Important ML kernel (SVM, kernel methods)

**Evolutionary Algorithm** (`evolutionary.rkt`)
- **Description:** Genetic algorithm for optimization
- **Input:** Population size, generations, problem instance
- **Parallelization:** Parallel fitness evaluation
- **Validation:** Convergence to known optima
- **Estimated effort:** 3-4 days
- **Value:** Tests irregular parallelism and selection/recombination patterns

### Phase 4: Effects and Advanced Features (Priority: LOW)

**Note:** OCaml 5.0 introduced algebraic effects, which don't have a direct Racket equivalent. These benchmarks may require adaptation or may not be portable.

**multicore-effects benchmarks:**
- May involve effect handlers, continuations, or other advanced control flow
- **Decision:** Defer until specific benchmark details are examined
- **Alternative:** Explore Racket's continuation-based patterns as substitutes

## Implementation Guidelines

### Directory Structure

```
benchmarks/
  sandmark/              # NEW - Sandmark-specific ports
    README.md            # Overview, attribution, usage
    matrix-mult.rkt
    lu-decomp.rkt
    floyd-warshall.rkt
    merge-sort.rkt
    quick-sort.rkt
    game-of-life.rkt
    grammatrix.rkt
    evolutionary.rkt
    minilight.rkt
    graph500-sssp.rkt
  concurrent-ds/         # NEW - Concurrent data structures
    README.md
    ms-queue.rkt
    spsc-queue.rkt
    ws-queue.rkt
    treiber-stack.rkt
    lf-hash.rkt
    lf-list.rkt
tests/
  sandmark/              # NEW
    matrix-mult-test.rkt
    lu-decomp-test.rkt
    ...
  concurrent-ds/         # NEW
    ms-queue-test.rkt
    ...
```

### Common Patterns for Sandmark Ports

#### 1. Standard CLI Interface

All benchmarks should use the shared `benchmarks/common/cli.rkt` infrastructure:

```racket
#lang racket
(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt")

(define n 1000)
(define workers 4)
(define repeats 3)
(define log-file #f)

(define (parse-args)
  (command-line
   #:program "benchmark-name"
   #:once-each
   [("--n") n-val "Problem size" (set! n (string->number n-val))]
   [("--workers") w "Worker count" (set! workers (string->number w))]
   [("--repeat") r "Repetitions" (set! repeats (string->number r))]
   [("--log") file "Log file" (set! log-file file)]))
```

#### 2. Sequential and Parallel Variants

Each benchmark should provide both implementations:

```racket
;; Sequential baseline
(define (algorithm-sequential input)
  ...)

;; Parallel implementation
(define (algorithm-parallel input workers)
  ...)
```

#### 3. Verification

Include correctness checks:

```racket
(define (verify-result seq-result par-result)
  (unless (equal? seq-result par-result)
    (error "Results differ!")))
```

#### 4. Racket-Specific Parallelism

Use appropriate Racket parallel primitives:

- **Futures** - for CPU-bound parallel computation
- **Threads** - for coordination and concurrent data structures
- **Places** - for true parallelism without GIL (if needed)
- **Custom thread pools** - use `benchmarks/common/parallel.rkt` utilities

#### 5. Attribution

Include clear attribution to Sandmark in README and source files:

```racket
;; Port of [benchmark-name] from the Sandmark OCaml benchmark suite
;; Original: https://github.com/ocaml-bench/sandmark
;; Adapted for Racket parallel benchmarking
```

### Testing Strategy

1. **Correctness tests:** Sequential vs parallel equivalence
2. **Small smoke tests:** Fast execution (< 1s)
3. **Known-answer tests:** Where applicable (e.g., known shortest paths)
4. **Scaling tests:** Verify speedup with increased workers
5. **Stress tests:** Large problem sizes for performance characterization

### Integration with Suite Runner

Add new benchmarks to `benchmarks/run-suite.rkt`:

```racket
(define sandmark-benchmarks
  '((matrix-mult (n 1000) (workers 1 2 4 8))
    (lu-decomp (n 500) (workers 1 2 4 8))
    ...))

(define concurrent-ds-benchmarks
  '((ms-queue (ops 1000000) (workers 2 4 8))
    ...))
```

Create config files in `benchmarks/config/`:
- `sandmark-quick.sexp` - Small problem sizes for CI
- `sandmark-standard.sexp` - Medium problem sizes
- `sandmark-stress.sexp` - Large problem sizes for thorough testing

## Priority and Timeline

### High Priority (Phase 1) - Target: 4-6 weeks

**Week 1-2:**
- Matrix multiplication
- Matrix tiling variant
- Merge sort

**Week 3-4:**
- LU decomposition
- Quick sort
- Floyd-Warshall

**Week 5-6:**
- Gram matrix
- Graph500 SSSP (if time permits)

**Deliverables:**
- 6-7 new benchmarks in `benchmarks/sandmark/`
- Tests for all benchmarks
- README with usage examples
- Integration with suite runner

### Medium Priority (Phase 2) - Target: 4-5 weeks

**Week 1-2:**
- MS Queue
- SPSC Queue
- Treiber Stack

**Week 3-4:**
- Work-stealing queue
- Lock-free hash table

**Week 5:**
- Lock-free list
- Testing and documentation

**Deliverables:**
- 6 new benchmarks in `benchmarks/concurrent-ds/`
- Comprehensive tests for concurrent correctness
- Performance comparison plots
- README with data structure descriptions

### Low Priority (Phase 3) - Target: 3-4 weeks

**Week 1-2:**
- Game of Life
- Evolutionary algorithm

**Week 3-4:**
- Minilight ray tracer
- Testing and optimization

**Deliverables:**
- 3 specialized benchmarks
- Visual outputs where applicable
- Documentation

## Technical Challenges and Considerations

### 1. Lock-Free Data Structures in Racket

**Challenge:** Racket doesn't expose low-level CAS (compare-and-swap) primitives directly.

**Solutions:**
- Use `(require racket/unsafe/ops)` for atomic operations
- Consider using Chez Scheme's FFI for CAS if needed
- Explore Racket's `ffi/unsafe/atomic` module
- Alternative: Use mutex-based implementations as "lock-free-style" with fine-grained locking

**Research needed:** Determine best approach for true lock-free implementations in Racket.

### 2. Memory Model and Visibility

**Challenge:** OCaml's memory model differs from Racket's.

**Solutions:**
- Use Racket's thread synchronization primitives (semaphores, channels)
- Ensure proper memory barriers with `sync` operations
- Test extensively for race conditions

### 3. Performance Differences

**Challenge:** Direct performance comparison with OCaml is not the goal, but we want meaningful benchmarks.

**Solutions:**
- Focus on relative speedup (sequential vs parallel in Racket)
- Document expected scaling characteristics
- Compare against other Racket parallel patterns

### 4. Random Number Generation

**Challenge:** Parallel algorithms need thread-safe RNG.

**Solutions:**
- Use `(require racket/random)` with separate generators per thread
- Provide `--seed` parameter for reproducibility
- Document RNG strategy in benchmark documentation

### 5. Floating-Point Precision

**Challenge:** Numerical algorithms may have precision differences.

**Solutions:**
- Use appropriate epsilon for floating-point comparisons
- Document expected precision in tests
- Consider using `math/bigfloat` for reference implementations

## Exclusions and Non-Ports

### Benchmarks NOT being ported:

1. **almabench, coq, alt-ergo, menhir, etc.** - Application-specific benchmarks tied to OCaml tooling
2. **decompress, yojson, irmin, cpdf** - Library-specific benchmarks
3. **multicore-effects** - OCaml 5.0 effect handlers (no direct Racket equivalent)
4. **Duplicate shootout benchmarks** - Already have spectral-norm, nbody, mandelbrot, chameneos

### Rationale:
Focus on **algorithmic benchmarks** that test parallel patterns and data structures, not OCaml-specific libraries or language features.

## Success Metrics

### Quantitative:
- **15-20 new benchmarks** across 2-3 new categories
- **100% test coverage** for correctness
- **Documentation** for all benchmarks
- **Integration** with suite runner and visualization tools

### Qualitative:
- Benchmarks demonstrate diverse parallel patterns
- Clear attribution to Sandmark
- Consistent with existing suite architecture
- Useful for Racket parallel performance evaluation

## References

- [Sandmark Repository](https://github.com/ocaml-bench/sandmark)
- [Sandmark Nightly Results](https://sandmark.tarides.com/)
- [Graph500 from OCaml-Multicore Perspective](https://arxiv.org/abs/2012.13548)
- [OCaml Multicore Documentation](https://ocaml.org/news/multicore)
- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)

## Appendix: Complete Sandmark Benchmark Inventory

### multicore-numerical (10 benchmarks)
- ✓ LU decomposition - **PORT** (Phase 1)
- ✓ Matrix multiplication - **PORT** (Phase 1)
- ✓ Matrix multiplication tiled - **PORT** (Phase 1)
- ✓ Merge sort - **PORT** (Phase 1)
- ✓ Quick sort - **PORT** (Phase 1)
- ✓ Floyd-Warshall - **PORT** (Phase 1)
- ✓ Game of Life - **PORT** (Phase 3)
- ✓ Evolutionary algorithm - **PORT** (Phase 3)
- ✗ Mandelbrot - **SKIP** (already in shootout)
- ✗ Nbody - **SKIP** (already in shootout)
- ✗ Spectral norm - **SKIP** (already in shootout)

### multicore-structures (3 benchmarks)
- ✓ MS Queue - **PORT** (Phase 2)
- ✓ SPSC Queue - **PORT** (Phase 2)
- ✓ Treiber Stack - **PORT** (Phase 2)

### multicore-lockfree (5 benchmarks)
- ✓ MS Queue - **PORT** (Phase 2)
- ✓ Work-stealing queue - **PORT** (Phase 2)
- ✓ Hash table - **PORT** (Phase 2)
- ✓ List - **PORT** (Phase 2)
- ✓ Bag - **PORT** (Phase 2)

### multicore-grammatrix (1 benchmark)
- ✓ Gram matrix - **PORT** (Phase 1)

### multicore-minilight (1 benchmark)
- ✓ Minilight ray tracer - **PORT** (Phase 3)

### graph500seq (2 benchmarks)
- ✗ BFS - **SKIP** (already in MPL)
- ✓ SSSP - **PORT** (Phase 1)

### benchmarksgame (subset)
- ✗ All covered - **SKIP** (already in shootout)

### Other categories
- ✗ Application benchmarks - **SKIP** (OCaml-specific)
- ✗ multicore-effects - **SKIP** (no Racket equivalent)

**Total to port: ~20 benchmarks**

---

## Next Steps

1. **Review this plan** with project stakeholders
2. **Set up directory structure** for new categories
3. **Begin Phase 1** with matrix multiplication as pilot implementation
4. **Research Racket CAS primitives** for lock-free data structures
5. **Create config files** for new benchmark suites
6. **Update documentation** (CLAUDE.md, BENCHMARKS.md, README.md)
