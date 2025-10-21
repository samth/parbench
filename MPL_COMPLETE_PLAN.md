# MPL Benchmark Suite - Complete Implementation Plan (ACCURATE)

## 🎯 Current Status (2025-10-21)

**22/38 MPL benchmarks implemented (57.9%)** | **Phase 4 & 5 COMPLETE ✅** | **Phase 3 PARTIAL ✅**

- ✅ **7 Core benchmarks:** bfs, convex-hull, histogram, integer-sort, mis, msf, suffix-array
- ✅ **6 Phase 5 (Toy/Utility):** fib, bignum-add, palindrome, shuffle, flatten, collect
- ✅ **5 Phase 3 (Algorithmic/Puzzle):** nqueens, mcss, subset-sum, parens, dedup
- ✅ **1 Phase 4 (Primes):** primes
- ✅ **3 Phase 1 (Text + Sorting):** tokens, word-count, merge-sort
- 🔜 **Next:** Phase 1 (High Priority) - 5 more graph/text benchmarks
- **Remaining:** 16 benchmarks across Phases 1-3

## Executive Summary

This document provides an **accurate** plan for implementing remaining benchmarks from the MPL (MaPLe) Parallel ML benchmark suite, based on the actual contents of https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench.

**Key Findings:**
- **MPL has 84 benchmarks** (not ~30 as initially thought)
- **13 already implemented** in this repository (7 core + 6 Phase 5)
- **Many are variants** (optimized versions, pure versions, different implementations)
- **Many are test/research benchmarks** (entanglement studies, fragmentation tests)
- **Core unique benchmarks:** ~30-35 distinct problems

## Complete MPL Benchmark Inventory

### Already Implemented (18 benchmarks)

#### Core MPL Benchmarks (7)
1. ✅ **bfs** - Breadth-first search (matches MPL's `bfs`)
2. ✅ **convex-hull** - 2D convex hull (matches MPL's `quickhull` or `pure-quickhull`)
3. ✅ **histogram** - Histogram counting (no direct MPL match, but similar to PBBS)
4. ✅ **integer-sort** - Integer sorting (similar to MPL's `msort-int32`)
5. ✅ **mis** - Maximal independent set (matches MPL's `max-indep-set`)
6. ✅ **msf** - Minimum spanning forest (no direct MPL match, PBBS-based)
7. ✅ **suffix-array** - Suffix array construction (matches MPL's `suffix-array`)

#### Phase 5: Toy/Utility Benchmarks (6) - COMPLETED
8. ✅ **fib** - Naive recursive parallel Fibonacci
9. ✅ **bignum-add** - Parallel addition of large integers
10. ✅ **palindrome** - Palindrome checking
11. ✅ **shuffle** - Parallel random shuffle
12. ✅ **flatten** - Flatten nested sequences
13. ✅ **collect** - Collect/filter operations

#### Phase 3: Algorithmic/Puzzle Benchmarks (5) - COMPLETED
14. ✅ **nqueens** - N-Queens backtracking
15. ✅ **mcss** - Maximum contiguous subsequence sum
16. ✅ **subset-sum** - Subset sum problem
17. ✅ **parens** - Parentheses matching
18. ✅ **dedup** - Deduplication

#### Phase 4: Primes (1) - COMPLETED
19. ✅ **primes** - Segmented sieve of Eratosthenes

#### Phase 1: Text Processing & Sorting (3 of 3 + 1 of 2) - IN PROGRESS
20. ✅ **tokens** - Parallel tokenization
21. ✅ **word-count** - Count lines, words, and bytes
22. ✅ **merge-sort** - Classic parallel merge sort

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

**Merge Sort** (`merge-sort.rkt`) ✅
- **MPL:** `msort`
- **Description:** Standard parallel merge sort
- **Value:** Classic parallel algorithm
- **Estimated effort:** 2-3 days
- **Status:** COMPLETED
- **Note:** Similar to Sandmark, coordinate

#### 1.3 Text Processing

**Tokens** (`tokens.rkt`) ✅
- **MPL:** `tokens`
- **Description:** Parallel tokenization
- **Value:** Text processing building block
- **Estimated effort:** 2-3 days
- **Status:** COMPLETED

**Word Count** (`word-count.rkt`) ✅
- **MPL:** `wc`
- **Description:** Count word frequencies
- **Value:** MapReduce classic
- **Estimated effort:** 2-3 days
- **Status:** COMPLETED

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

**N-Queens** (`nqueens.rkt`) ✅
- **MPL:** `nqueens`
- **Description:** N-queens backtracking
- **Value:** Constraint satisfaction
- **Estimated effort:** 3-4 days
- **Status:** COMPLETED

**Maximum Contiguous Subsequence Sum** (`mcss.rkt`) ✅
- **MPL:** `mcss`
- **Description:** MCSS problem (1D max subarray)
- **Value:** Classic DP problem, parallel prefix patterns
- **Estimated effort:** 2 days
- **Status:** COMPLETED

**Subset Sum** (`subset-sum.rkt`) ✅
- **MPL:** `subset-sum`
- **Description:** Subset sum problem
- **Value:** Classic NP-complete problem
- **Estimated effort:** 3-4 days
- **Status:** COMPLETED

#### 3.3 Sequence Problems

**Parentheses Matching** (`parens.rkt`) ✅
- **MPL:** `parens`
- **Description:** Parallel paren matching/balancing
- **Value:** Tests parallel prefix operations
- **Estimated effort:** 2 days
- **Status:** COMPLETED

**Deduplication** (`dedup.rkt`) ✅
- **MPL:** `dedup`
- **Description:** Remove duplicates from sequence
- **Value:** Data cleaning operation
- **Estimated effort:** 2 days
- **Status:** COMPLETED

### Phase 4: Primes & Specialized (Priority: LOW)

**Primes - Segmented Sieve** (`primes.rkt`) ✅
- **MPL:** `primes-segmented`
- **Description:** Segmented sieve of Eratosthenes
- **Value:** Classic parallel algorithm
- **Estimated effort:** 3-4 days
- **Status:** COMPLETED

**Interval Tree** (`interval-tree.rkt`)
- **MPL:** `interval-tree`
- **Description:** Interval tree operations
- **Value:** Computational geometry
- **Estimated effort:** 4-5 days

### Phase 5: Toy/Utility Benchmarks (Priority: VERY LOW)

These are simple benchmarks useful for testing infrastructure, debugging, and educational purposes.

**Fibonacci** (`fib.rkt`) ✅
- **MPL:** `fib`
- **Description:** Naive recursive parallel Fibonacci
- **Value:** Trivial parallelism test, good for debugging scheduler
- **Estimated effort:** 1 day
- **Status:** COMPLETED

**Big Number Addition** (`bignum-add.rkt`) ✅
- **MPL:** `bignum-add`
- **Description:** Parallel addition of large integers
- **Value:** Tests chunking and carry propagation
- **Estimated effort:** 2 days
- **Status:** COMPLETED

**Palindrome** (`palindrome.rkt`) ✅
- **MPL:** `palindrome`
- **Description:** Palindrome checking
- **Value:** Simple string algorithm
- **Estimated effort:** 1 day
- **Status:** COMPLETED

**Shuffle** (`shuffle.rkt`) ✅
- **MPL:** `shuf`
- **Description:** Parallel random shuffle
- **Value:** Tests randomization and permutation
- **Estimated effort:** 1-2 days
- **Status:** COMPLETED

**Flatten** (`flatten.rkt`) ✅
- **MPL:** `flatten`
- **Description:** Flatten nested sequences
- **Value:** Tests sequence operations
- **Estimated effort:** 1 day
- **Status:** COMPLETED

**Collect** (`collect.rkt`) ✅
- **MPL:** `collect`
- **Description:** Collect/filter operations
- **Value:** Tests parallel filtering
- **Estimated effort:** 1 day
- **Status:** COMPLETED

## Benchmarks NOT to Port

### Variants (pick one implementation, skip rest)
- Skip `*-opt` variants (optimization studies specific to MPL)
- Skip `pure-*` variants (purity studies specific to MPL)
- Skip `*-entangled` variants (MPL GC research)

### Already Covered
- Skip `ocaml-*` benchmarks (already have nbody, mandelbrot, binary-trees in shootout)

### Toy/Test Benchmarks - NOW INCLUDED
- ✓ Include `fib` (Phase 5 - useful for testing)
- ✓ Include `bignum-add` (Phase 5 - tests carry propagation)
- ✓ Include `palindrome` (Phase 5 - simple string algorithm)
- Skip `random` (not a real benchmark, just RNG)
- Skip `high-frag` (MPL-specific fragmentation test)

### Utility/Infrastructure
- Skip `graphio` (utility, not benchmark)
- Skip `to-gif`, `gif-encode` (image format conversion, not interesting algorithms)
- ✓ Include `collect`, `flatten`, `shuf` (Phase 5 - simple but useful utilities)

### Audio (unless we want media processing)
- Skip `reverb`, `tape-delay` (niche, complex DSP)

## Summary of Porting Plan

### By Priority

**High Priority (Phase 1): 8 benchmarks** - 3/8 COMPLETED ✅
- connectivity, triangle-count, centrality (graphs) - NOT STARTED (0/3)
- samplesort (sorting) - NOT STARTED, merge-sort ✅ (1/2)
- tokens ✅, word-count ✅, grep (text) - 2/3 COMPLETED

**Medium-High Priority (Phase 2): 8 benchmarks**
- delaunay, nearest-nbrs, skyline, range-tree (geometry)
- dense-matmul, sparse-mxv, line-fit, integrate (numerical)

**Medium Priority (Phase 3): 7 benchmarks** - 5/7 COMPLETED ✅
- seam-carve, raytracer (image/graphics) - NOT STARTED
- nqueens ✅, mcss ✅, subset-sum ✅ (optimization) - COMPLETED
- parens ✅, dedup ✅ (sequences) - COMPLETED

**Low Priority (Phase 4): 2 benchmarks** - 1/2 COMPLETED ✅
- primes ✅ - COMPLETED
- interval-tree - NOT STARTED

**Very Low Priority (Phase 5): 6 benchmarks** ✅ **COMPLETED**
- fib ✅, bignum-add ✅, palindrome ✅, shuffle ✅, flatten ✅, collect ✅

**Total to port: 31 benchmarks** (from 84 MPL benchmarks, 7 already done)
**Phase 5 COMPLETED: 6/6 benchmarks implemented** ✅
**Phase 4 MOSTLY COMPLETE: 1/2 benchmarks implemented** ✅
**Phase 3 (Algorithmic/Puzzle) MOSTLY COMPLETE: 5/7 benchmarks implemented** ✅
**Phase 1 IN PROGRESS: 3/8 benchmarks implemented** (37.5%)
**Overall Progress: 22/38 MPL benchmarks (57.9%)**

### Implementation Timeline

**Phase 1 (High):** 8 benchmarks, ~6-8 weeks - ✅ **3/8 COMPLETED** (tokens, word-count, merge-sort)
**Phase 2 (Medium-High):** 8 benchmarks, ~7-9 weeks - NOT STARTED
**Phase 3 (Medium):** 7 benchmarks, ~6-7 weeks - ✅ **5/7 COMPLETED** (nqueens, mcss, subset-sum, parens, dedup)
**Phase 4 (Low):** 2 benchmarks, ~2-3 weeks - ✅ **1/2 COMPLETED** (primes)
**Phase 5 (Very Low):** 6 benchmarks, ~1-2 weeks - ✅ **COMPLETED**

**Total estimated effort:** ~22-29 weeks (5.5-7.5 months) for all 31 benchmarks
**Progress:** 15/31 benchmarks completed (48.4%)**

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
  # Existing (7) - ✅ ALL COMPLETED
  bfs.rkt ✅
  convex-hull.rkt ✅
  histogram.rkt ✅
  integer-sort.rkt ✅
  mis.rkt ✅
  msf.rkt ✅
  suffix-array.rkt ✅
  # Phase 1 - High Priority (8) - 3/8 COMPLETED
  connectivity.rkt
  triangle-count.rkt
  centrality.rkt
  samplesort.rkt
  merge-sort.rkt ✅
  tokens.rkt ✅
  word-count.rkt ✅
  grep.rkt
  # Phase 2 - Geometry & Numerical (8) - NOT STARTED
  delaunay.rkt
  knn.rkt
  skyline.rkt
  range-tree.rkt
  dense-matmul.rkt
  sparse-mxv.rkt
  line-fit.rkt
  integrate.rkt
  # Phase 3 - Specialized (7) - 5/7 COMPLETED
  seam-carve.rkt
  raytracer.rkt
  nqueens.rkt ✅
  mcss.rkt ✅
  subset-sum.rkt ✅
  parens.rkt ✅
  dedup.rkt ✅
  # Phase 4 - Primes & Advanced (2) - 1/2 COMPLETED
  primes.rkt ✅
  interval-tree.rkt
  # Phase 5 - Toy/Utility (6) - ✅ ALL COMPLETED
  fib.rkt ✅
  bignum-add.rkt ✅
  palindrome.rkt ✅
  shuffle.rkt ✅
  flatten.rkt ✅
  collect.rkt ✅
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
- 31 new MPL benchmarks to implement (15/31 completed = 48.4%)
- Total MPL suite target: 38 benchmarks (22 implemented: 7 core + 6 Phase 5 + 5 Phase 3 + 1 Phase 4 + 3 Phase 1)
- 100% test coverage (✅ all 22 implemented benchmarks have tests)
- All benchmarks integrated with suite runner
- ✅ Phase 5 complete: toy/utility benchmarks for testing infrastructure (6/6)
- ✅ Phase 4 mostly complete: primes benchmark (1/2)
- ✅ Phase 3 mostly complete: algorithmic/puzzle benchmarks (5/7)
- 🔄 Phase 1 in progress: text processing + sorting (3/8, 37.5%)

**Qualitative:**
- Comprehensive coverage of MPL's diverse algorithm domains (in progress)
- ✅ Clear attribution to MPL
- ✅ Consistent with existing architecture
- Valuable for Racket parallel performance evaluation
- ✅ Complete range from complex (convex-hull, msf) to simple (fib, palindrome)

## References

- [MPL parallel-ml-bench](https://github.com/MPLLang/parallel-ml-bench)
- [MPL Compiler](https://github.com/MPLLang/mpl)
- [POPL 2024: Automatic Parallelism Management](https://cs.nyu.edu/~shw8119/24/popl24-par-manage.pdf)
- [POPL 2021: Provably Space-Efficient Parallel Functional Programming](https://dl.acm.org/doi/10.1145/3434299)
- [ICFP 2022: Entanglement Detection with Near-Zero Cost](https://dl.acm.org/doi/10.1145/3547646)

## Next Steps

1. ✅ **VERIFIED: Accessed actual MPL repository** - Found 84 benchmarks
2. ✅ **Phase 5 COMPLETED** - All 6 toy/utility benchmarks implemented with tests
3. ✅ **Phase 3 (Algorithmic/Puzzle) MOSTLY COMPLETE** - 5/7 benchmarks implemented (nqueens, mcss, subset-sum, parens, dedup)
4. **Begin Phase 1** with connectivity and triangle-count as pilots
5. **Update benchmarks/mpl/README.md** to list new benchmarks as added
6. **Coordinate with Sandmark plan** for dense-matmul
7. **Create test infrastructure** for new algorithm categories
8. **Retire MPL_REMAINING_PLAN.md** (replaced by this accurate plan)

## Current Status (2025-10-21)

- ✅ **22 MPL benchmarks implemented** (7 core + 6 Phase 5 + 5 Phase 3 + 1 Phase 4 + 3 Phase 1)
- ✅ **All 22 have passing tests** (18 total test files with multiple test cases each)
- ✅ **Phase 5 (Toy/Utility) COMPLETE:** fib, bignum-add, palindrome, shuffle, flatten, collect
- ✅ **Phase 4 MOSTLY COMPLETE:** primes (1/2)
- ✅ **Phase 3 (Algorithmic/Puzzle) MOSTLY COMPLETE:** nqueens, mcss, subset-sum, parens, dedup (5/7)
- ✅ **Phase 1 IN PROGRESS:** tokens, word-count, merge-sort (3/8, 37.5%)
- 🔜 **Next:** Continue Phase 1 (grep, connectivity, triangle-count, centrality, samplesort) OR finish Phase 3 (seam-carve, raytracer)
- **Remaining:** 16 benchmarks across Phases 1-3
