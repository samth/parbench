# MPL Benchmark Suite - Complete Implementation Plan (ACCURATE)

## Executive Summary

This document provides an **accurate** plan for implementing remaining benchmarks from the MPL (MaPLe) Parallel ML benchmark suite, based on the actual contents of https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench.

**Key Findings:**
- **MPL has 84 benchmarks** (not ~30 as initially thought)
- **7 already implemented** in this repository
- **Many are variants** (optimized versions, pure versions, different implementations)
- **Many are test/research benchmarks** (entanglement studies, fragmentation tests)
- **Core unique benchmarks:** ~30-35 distinct problems

## Complete MPL Benchmark Inventory

### Already Implemented (7 benchmarks)

1. ✅ **bfs** - Breadth-first search (matches MPL's `bfs`)
2. ✅ **convex-hull** - 2D convex hull (matches MPL's `quickhull` or `pure-quickhull`)
3. ✅ **histogram** - Histogram counting (no direct MPL match, but similar to PBBS)
4. ✅ **integer-sort** - Integer sorting (similar to MPL's `msort-int32`)
5. ✅ **mis** - Maximal independent set (matches MPL's `max-indep-set`)
6. ✅ **msf** - Minimum spanning forest (no direct MPL match, PBBS-based)
7. ✅ **suffix-array** - Suffix array construction (matches MPL's `suffix-array`)

### All 84 MPL Benchmarks (Categorized)

#### **Graph Algorithms (15 benchmarks)**
- `bfs` ✅ (implemented)
- `bfs-delayed`, `bfs-det-dedup`, `bfs-det-priority` (BFS variants)
- `bfs-tree-entangled-fixed`, `bfs-tree-entangled` (entanglement research)
- `centrality` - Betweenness centrality
- `connectivity` - Graph connectivity
- `max-indep-set` ✅ (implemented as `mis`)
- `spanner` - Graph spanner
- `triangle-count` - Triangle counting
- `graphio` - Graph I/O utilities (not a benchmark)

#### **Sorting & Sequences (13 benchmarks)**
- `msort`, `msort-int32`, `msort-strings` - Merge sort variants
- `pure-msort`, `pure-msort-int32`, `pure-msort-strings` - Pure functional merge sort
- `samplesort` - Sample sort
- `dedup`, `dedup-entangled`, `dedup-entangled-fixed` - Deduplication
- `flatten` - Sequence flattening
- `shuf` - Shuffling
- `collect` - Collection operations

#### **Text Processing (5 benchmarks)**
- `suffix-array` ✅ (implemented)
- `tokens` - Tokenization
- `wc`, `wc-opt` - Word count
- `grep`, `grep-old` - Pattern matching

#### **Computational Geometry (12 benchmarks)**
- `delaunay`, `delaunay-top-down`, `delaunay-mostly-pure`, `delaunay-animation` - Delaunay triangulation
- `quickhull`, `pure-quickhull` ✅ (implemented as `convex-hull`)
- `nearest-nbrs`, `pure-nn` - Nearest neighbors
- `skyline`, `pure-skyline` - Skyline problem
- `low-d-decomp` - Low-dimensional decomposition
- `range-tree` - Range tree queries

#### **Numerical & Scientific (10 benchmarks)**
- `dense-matmul` - Dense matrix multiplication
- `sparse-mxv`, `sparse-mxv-opt` - Sparse matrix-vector multiply
- `integrate`, `integrate-opt` - Numerical integration
- `linefit`, `linefit-opt` - Line fitting / linear regression
- `raytracer` - Ray tracing
- `tinykaboom` - Another ray tracer

#### **OCaml Comparison Benchmarks (8 benchmarks)**
These are ports from OCaml's Sandmark for comparison:
- `ocaml-binarytrees5` - Binary trees
- `ocaml-game-of-life`, `ocaml-game-of-life-pure` - Game of Life
- `ocaml-lu-decomp` - LU decomposition
- `ocaml-mandelbrot` - Mandelbrot set
- `ocaml-nbody`, `ocaml-nbody-imm`, `ocaml-nbody-packed` - N-body simulation

#### **Image/Media Processing (6 benchmarks)**
- `seam-carve`, `seam-carve-index` - Seam carving
- `gif-encode` - GIF encoding
- `to-gif` - Convert to GIF
- `reverb` - Audio reverb effect
- `tape-delay` - Audio tape delay

#### **Algorithmic Puzzles & Optimization (7 benchmarks)**
- `nqueens`, `nqueens-simple` - N-queens problem
- `parens` - Parentheses matching
- `palindrome` - Palindrome checking
- `mcss`, `mcss-opt` - Maximum contiguous subsequence sum
- `subset-sum` - Subset sum problem

#### **Toy/Test Benchmarks (5 benchmarks)**
- `fib` - Fibonacci (trivial parallelism test)
- `random` - Random number generation
- `bignum-add`, `bignum-add-opt` - Big number addition
- `high-frag` - Fragmentation test

#### **Specialized/Research (3 benchmarks)**
- `interval-tree` - Interval tree operations
- `linearrec`, `linearrec-opt` - Linear recurrence

#### **Primes (3 benchmarks)**
- `primes` - Basic sieve
- `primes-blocked` - Blocked sieve
- `primes-segmented` - Segmented sieve

## Analysis: What to Implement

### Core Missing Benchmarks Worth Porting (Priority Order)

Many MPL benchmarks are:
- **Variants** (opt, pure, entangled versions) - we only need one
- **OCaml comparisons** - already covered in our shootout suite
- **Toy benchmarks** (fib, random) - not interesting
- **Research-specific** (entanglement, fragmentation) - MPL-specific concerns

**Focus on distinct, valuable algorithms not yet in our suite.**

### Phase 1: High-Value Core Algorithms (Priority: HIGH)

#### 1.1 Essential Graph Algorithms

**Connectivity** (`connectivity.rkt`)
- **MPL:** `connectivity`
- **Description:** Compute connected components
- **Value:** Fundamental graph algorithm, complements BFS/MIS
- **Estimated effort:** 3-4 days

**Triangle Counting** (`triangle-count.rkt`)
- **MPL:** `triangle-count`
- **Description:** Count triangles in graph
- **Value:** Important for social network analysis
- **Estimated effort:** 3-4 days

**Betweenness Centrality** (`centrality.rkt`)
- **MPL:** `centrality`
- **Description:** Compute vertex centrality
- **Value:** Key graph metric
- **Estimated effort:** 4-5 days

#### 1.2 Sorting Algorithms

**Sample Sort** (`samplesort.rkt`)
- **MPL:** `samplesort`
- **Description:** Parallel sample sort (compare-based)
- **Value:** Better parallel sort than merge sort for many inputs
- **Estimated effort:** 3-4 days

**Merge Sort** (`merge-sort.rkt`)
- **MPL:** `msort`
- **Description:** Standard parallel merge sort
- **Value:** Classic parallel algorithm
- **Estimated effort:** 2-3 days
- **Note:** Similar to Sandmark, coordinate

#### 1.3 Text Processing

**Tokens** (`tokens.rkt`)
- **MPL:** `tokens`
- **Description:** Parallel tokenization
- **Value:** Text processing building block
- **Estimated effort:** 2-3 days

**Word Count** (`word-count.rkt`)
- **MPL:** `wc`
- **Description:** Count word frequencies
- **Value:** MapReduce classic
- **Estimated effort:** 2-3 days

**Grep** (`grep.rkt`)
- **MPL:** `grep`
- **Description:** Parallel pattern matching
- **Value:** String search algorithm
- **Estimated effort:** 3-4 days

### Phase 2: Geometry & Numerical (Priority: MEDIUM-HIGH)

#### 2.1 Geometry

**Delaunay Triangulation** (`delaunay.rkt`)
- **MPL:** `delaunay`
- **Description:** 2D Delaunay triangulation
- **Value:** Important geometry algorithm
- **Estimated effort:** 6-8 days
- **Note:** Complex, use top-down variant as reference

**Nearest Neighbors** (`knn.rkt`)
- **MPL:** `nearest-nbrs`
- **Description:** K-nearest neighbors
- **Value:** Spatial query, ML algorithm
- **Estimated effort:** 5-6 days

**Skyline** (`skyline.rkt`)
- **MPL:** `skyline`
- **Description:** Skyline/maximal points problem
- **Value:** Computational geometry, databases
- **Estimated effort:** 3-4 days

**Range Tree** (`range-tree.rkt`)
- **MPL:** `range-tree`
- **Description:** Range queries in 2D
- **Value:** Spatial data structure
- **Estimated effort:** 5-6 days

#### 2.2 Numerical

**Dense Matrix Multiply** (`dense-matmul.rkt`)
- **MPL:** `dense-matmul`
- **Description:** Matrix multiplication
- **Value:** Linear algebra fundamental
- **Estimated effort:** 2-3 days
- **Note:** Coordinate with Sandmark to avoid duplication

**Sparse Matrix-Vector Multiply** (`sparse-mxv.rkt`)
- **MPL:** `sparse-mxv`
- **Description:** Sparse MxV product
- **Value:** Iterative solver kernel
- **Estimated effort:** 3-4 days

**Line Fitting** (`line-fit.rkt`)
- **MPL:** `linefit`
- **Description:** Linear regression / line fitting
- **Value:** Statistics, ML
- **Estimated effort:** 2-3 days

**Numerical Integration** (`integrate.rkt`)
- **MPL:** `integrate`
- **Description:** Parallel numerical integration
- **Value:** Scientific computing
- **Estimated effort:** 2 days

### Phase 3: Specialized Algorithms (Priority: MEDIUM)

#### 3.1 Image/Media

**Seam Carving** (`seam-carve.rkt`)
- **MPL:** `seam-carve`
- **Description:** Content-aware image resizing
- **Value:** Image processing algorithm
- **Estimated effort:** 4-5 days

**Ray Tracer** (`raytracer.rkt`)
- **MPL:** `raytracer`
- **Description:** Simple ray tracer
- **Value:** Graphics algorithm
- **Estimated effort:** 6-7 days

#### 3.2 Optimization Problems

**N-Queens** (`nqueens.rkt`)
- **MPL:** `nqueens`
- **Description:** N-queens backtracking
- **Value:** Constraint satisfaction
- **Estimated effort:** 3-4 days

**Maximum Contiguous Subsequence Sum** (`mcss.rkt`)
- **MPL:** `mcss`
- **Description:** MCSS problem (1D max subarray)
- **Value:** Classic DP problem, parallel prefix patterns
- **Estimated effort:** 2 days

**Subset Sum** (`subset-sum.rkt`)
- **MPL:** `subset-sum`
- **Description:** Subset sum problem
- **Value:** Classic NP-complete problem
- **Estimated effort:** 3-4 days

#### 3.3 Sequence Problems

**Parentheses Matching** (`parens.rkt`)
- **MPL:** `parens`
- **Description:** Parallel paren matching/balancing
- **Value:** Tests parallel prefix operations
- **Estimated effort:** 2 days

**Deduplication** (`dedup.rkt`)
- **MPL:** `dedup`
- **Description:** Remove duplicates from sequence
- **Value:** Data cleaning operation
- **Estimated effort:** 2 days

### Phase 4: Primes & Specialized (Priority: LOW)

**Primes - Segmented Sieve** (`primes.rkt`)
- **MPL:** `primes-segmented`
- **Description:** Segmented sieve of Eratosthenes
- **Value:** Classic parallel algorithm
- **Estimated effort:** 3-4 days

**Interval Tree** (`interval-tree.rkt`)
- **MPL:** `interval-tree`
- **Description:** Interval tree operations
- **Value:** Computational geometry
- **Estimated effort:** 4-5 days

## Benchmarks NOT to Port

### Variants (pick one implementation, skip rest)
- Skip `*-opt` variants (optimization studies specific to MPL)
- Skip `pure-*` variants (purity studies specific to MPL)
- Skip `*-entangled` variants (MPL GC research)

### Already Covered
- Skip `ocaml-*` benchmarks (already have nbody, mandelbrot, binary-trees in shootout)

### Toy/Test Benchmarks
- Skip `fib` (trivial)
- Skip `random` (not a real benchmark)
- Skip `high-frag` (MPL-specific fragmentation test)
- Skip `bignum-add` (too simple)

### Utility/Infrastructure
- Skip `graphio` (utility, not benchmark)
- Skip `to-gif`, `gif-encode` (image format conversion, not interesting algorithms)
- Skip `collect`, `flatten`, `shuf` (too simple, test utilities)

### Audio (unless we want media processing)
- Skip `reverb`, `tape-delay` (niche, complex DSP)

## Summary of Porting Plan

### By Priority

**High Priority (Phase 1): 8 benchmarks**
- connectivity, triangle-count, centrality (graphs)
- samplesort, merge-sort (sorting)
- tokens, word-count, grep (text)

**Medium-High Priority (Phase 2): 8 benchmarks**
- delaunay, nearest-nbrs, skyline, range-tree (geometry)
- dense-matmul, sparse-mxv, line-fit, integrate (numerical)

**Medium Priority (Phase 3): 7 benchmarks**
- seam-carve, raytracer (image/graphics)
- nqueens, mcss, subset-sum (optimization)
- parens, dedup (sequences)

**Low Priority (Phase 4): 2 benchmarks**
- primes, interval-tree

**Total to port: 25 benchmarks** (from 84 MPL benchmarks, 7 already done)

### Implementation Timeline

**Phase 1 (High):** 8 benchmarks, ~6-8 weeks
**Phase 2 (Medium-High):** 8 benchmarks, ~7-9 weeks
**Phase 3 (Medium):** 7 benchmarks, ~6-7 weeks
**Phase 4 (Low):** 2 benchmarks, ~2-3 weeks

**Total estimated effort:** ~21-27 weeks (5-7 months) for all 25 benchmarks

## Implementation Guidelines

### Standard Structure

Continue using existing MPL benchmark patterns from `benchmarks/mpl/`:

```racket
#lang racket
(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(define (benchmark-sequential input)
  ...)

(define (benchmark-parallel input workers)
  ...)

(module+ main
  (define args (parse-args))
  (run-and-log args))
```

### Attribution

```racket
;; Port of [benchmark-name] from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/[name]
;; Adapted for Racket parallel benchmarking
```

### Testing

Each benchmark needs:
1. Correctness tests (seq vs parallel)
2. Smoke tests (< 1s)
3. Known-answer tests where applicable

### Directory Structure

All in `benchmarks/mpl/`:

```
benchmarks/mpl/
  README.md                # Update with new benchmarks
  # Existing (7)
  bfs.rkt
  convex-hull.rkt
  histogram.rkt
  integer-sort.rkt
  mis.rkt
  msf.rkt
  suffix-array.rkt
  # Phase 1 - High Priority (8)
  connectivity.rkt
  triangle-count.rkt
  centrality.rkt
  samplesort.rkt
  merge-sort.rkt
  tokens.rkt
  word-count.rkt
  grep.rkt
  # Phase 2 - Geometry & Numerical (8)
  delaunay.rkt
  knn.rkt
  skyline.rkt
  range-tree.rkt
  dense-matmul.rkt
  sparse-mxv.rkt
  line-fit.rkt
  integrate.rkt
  # Phase 3 - Specialized (7)
  seam-carve.rkt
  raytracer.rkt
  nqueens.rkt
  mcss.rkt
  subset-sum.rkt
  parens.rkt
  dedup.rkt
  # Phase 4 - Primes & Advanced (2)
  primes.rkt
  interval-tree.rkt
```

## Coordination with Other Plans

### With Sandmark Plan

**Overlaps to coordinate:**
1. **Dense matrix multiply** - Implement in Sandmark (matrix-mult.rkt)
2. **Merge sort / Quick sort** - Implement both (different algorithms)
3. **Primes** - Implement in MPL (segmented sieve)

### With Existing Suites

**Shootout benchmarks** already cover:
- N-body (skip MPL's ocaml-nbody)
- Mandelbrot (skip MPL's ocaml-mandelbrot)
- Binary trees (skip MPL's ocaml-binarytrees5)

**NAS benchmarks** already cover:
- Sparse matrix operations (IS benchmark)

## Success Metrics

**Quantitative:**
- 25 new MPL benchmarks implemented
- Total MPL suite: 32 benchmarks (7 existing + 25 new)
- 100% test coverage
- All benchmarks integrated with suite runner

**Qualitative:**
- Comprehensive coverage of MPL's diverse algorithm domains
- Clear attribution to MPL
- Consistent with existing architecture
- Valuable for Racket parallel performance evaluation

## References

- [MPL parallel-ml-bench](https://github.com/MPLLang/parallel-ml-bench)
- [MPL Compiler](https://github.com/MPLLang/mpl)
- [POPL 2024: Automatic Parallelism Management](https://cs.nyu.edu/~shw8119/24/popl24-par-manage.pdf)
- [POPL 2021: Provably Space-Efficient Parallel Functional Programming](https://dl.acm.org/doi/10.1145/3434299)
- [ICFP 2022: Entanglement Detection with Near-Zero Cost](https://dl.acm.org/doi/10.1145/3547646)

## Next Steps

1. ✅ **VERIFIED: Accessed actual MPL repository** - Found 84 benchmarks
2. **Begin Phase 1** with connectivity and triangle-count as pilots
3. **Update benchmarks/mpl/README.md** to list new benchmarks as added
4. **Coordinate with Sandmark plan** for dense-matmul
5. **Create test infrastructure** for new algorithm categories
6. **Retire MPL_REMAINING_PLAN.md** (replaced by this accurate plan)
