# MPL Benchmark Suite - Remaining Benchmarks Implementation Plan

## Executive Summary

This document outlines a plan to add the **remaining benchmarks** from the MPL (MaPLe) Parallel ML benchmark suite to this Racket parallel benchmarking repository.

**Important Note:** This plan is primarily based on the **PBBS (Problem-Based Benchmark Suite)** V2, which the MPL suite is stated to be derived from. While MPL documentation indicates the suite contains ~30 benchmarks ported from PBBS, ParlayLib, Ligra, and PAM, **we were unable to access the exact contents of the MPL parallel-ml-bench repository directly**. Therefore, this plan uses PBBS as the authoritative source and makes reasonable assumptions about MPL's benchmark selection.

We have already implemented **7 benchmarks** based on PBBS; this plan covers the **remaining ~15-18 benchmarks** that are suitable for Racket porting.

**MPL Repository:** https://github.com/MPLLang/parallel-ml-bench (exact benchmark list not confirmed)
**PBBS Repository:** https://github.com/cmuparlay/pbbsbench (confirmed source)

### Methodology & Limitations

- **Source:** PBBS V2 benchmark suite (22 core benchmarks documented at https://cmuparlay.github.io/pbbsbench/)
- **Assumption:** MPL parallel-ml-bench includes most/all PBBS benchmarks
- **Uncertainty:** MPL-specific benchmarks (tokens, dmm, primes) mentioned in commit messages and documentation but not directly verified
- **Verification needed:** Before implementing Phase 2-4, should attempt to access MPL repository directly or contact maintainers for accurate benchmark list

## Current State Analysis

### Already Implemented in benchmarks/mpl/ (7 benchmarks)

1. **histogram** ✓ - Histogram of integer sequences
2. **integer-sort** ✓ - Counting sort / radix sort for integers
3. **bfs** ✓ - Breadth-first search on graphs
4. **mis** ✓ - Maximal independent set
5. **msf** ✓ - Minimum spanning forest
6. **suffix-array** ✓ - Suffix array construction
7. **convex-hull** ✓ - Convex hull in 2D

### Already Implemented Elsewhere

8. **nbody** ✓ - In `benchmarks/shootout/nbody.rkt` (N-body simulation)
9. **spectral-norm** ✓ - In `benchmarks/shootout/spectral-norm.rkt` (eigenvalue approximation)

### PBBS/MPL Benchmark Inventory (22 core benchmarks)

Based on the PBBS V2 benchmark suite (which MPL is based on), here's the complete inventory:

#### **Sequences (4 benchmarks)**
- comparison-sort (SORT) - Comparison-based sorting
- histogram (HIST) - ✓ **DONE**
- integer-sort (ISORT) - ✓ **DONE**
- remove-duplicates (DDUP) - Remove duplicates from sequence

#### **Graphs (5 benchmarks)**
- breadth-first-search (BFS) - ✓ **DONE**
- maximal-independent-set (MIS) - ✓ **DONE**
- maximal-matching (MM) - Maximal matching in undirected graph
- min-spanning-forest (MSF) - ✓ **DONE**
- spanning-forest (SF) - Spanning tree/forest

#### **Text Processing (5 benchmarks)**
- bw-decode (BWD) - Burrows-Wheeler decode
- inverted-index (IIDX) - Inverted index construction
- longest-repeated-substring (LRS) - Find longest repeated substring
- suffix-array (SA) - ✓ **DONE**
- word-counts (WC) - Count word occurrences

#### **Geometry (6 benchmarks)**
- convex-hull (CH) - ✓ **DONE**
- delaunay-refine (DR) - Refine Delaunay triangulation
- delaunay-triangulation (DT) - Construct Delaunay triangulation
- nearest-neighbors (KNN) - K-nearest neighbors search
- ray-cast (RAY) - Ray-triangle intersection
- range-query-2d (RQ) - 2D range queries

#### **Other (2 benchmarks)**
- classify (CLAS) - Classification/prediction
- nbody (NBODY) - ✓ **DONE** (in shootout)

### MPL-Specific Additions (beyond PBBS)

MPL's parallel-ml-bench also includes additional benchmarks from other sources:

#### **Text/Sequence Processing**
- **tokens** - Tokenization of text
- **primes** - Prime number generation (Sieve of Eratosthenes)

#### **Numerical**
- **dmm** - Dense matrix multiplication

#### **Audio Processing**
- Audio filtering benchmarks (specific names TBD)

## Porting Priority Analysis

### Summary of Remaining Work

**Total PBBS benchmarks:** 22
**Already implemented:** 9 (7 in mpl/ + 2 in shootout/)
**Remaining from PBBS:** 13 benchmarks
**Additional MPL benchmarks:** ~3-5 benchmarks (tokens, primes, dmm, etc.)
**Total to port:** ~15-18 benchmarks

## Implementation Plan

### Phase 1: High-Priority Sequence & Graph Algorithms (Priority: HIGH)

These benchmarks fill important gaps in sequence and graph algorithm coverage.

#### 1.1 Sequence Algorithms

**Comparison Sort** (`comparison-sort.rkt`)
- **Description:** General comparison-based parallel sorting (merge sort or sample sort)
- **Input:** Sequence of comparable elements
- **Parallelization:** Parallel merge sort or parallel sample sort
- **Validation:** Verify sorted output, compare with sequential sort
- **Estimated effort:** 2-3 days
- **Value:** Complements integer-sort with general sorting, important baseline
- **Note:** Different from quicksort (in Sandmark plan) - this is merge sort or sample sort

**Remove Duplicates** (`remove-duplicates.rkt`)
- **Description:** Remove duplicate elements from a sequence
- **Input:** Sequence of elements
- **Parallelization:** Parallel sort + parallel compaction
- **Validation:** No duplicates in output, all unique elements preserved
- **Estimated effort:** 2 days
- **Value:** Tests parallel filtering/compaction patterns

#### 1.2 Graph Algorithms

**Maximal Matching** (`maximal-matching.rkt`)
- **Description:** Find maximal matching in undirected graph
- **Input:** Graph (adjacency list)
- **Output:** Set of edges forming maximal matching
- **Parallelization:** Randomized parallel algorithm (similar to MIS)
- **Validation:** Verify no two edges share a vertex, maximality
- **Estimated effort:** 3-4 days
- **Value:** Important graph algorithm, complements MIS

**Spanning Forest** (`spanning-forest.rkt`)
- **Description:** Find spanning tree (or forest if disconnected)
- **Input:** Undirected graph
- **Output:** Edges forming spanning tree/forest
- **Parallelization:** Parallel BFS or parallel connectivity
- **Validation:** Tree properties, all vertices reachable
- **Estimated effort:** 2-3 days
- **Value:** Simpler variant of MSF (unweighted), tests connectivity algorithms
- **Note:** May be redundant with MSF; consider lower priority

### Phase 2: Text Processing Benchmarks (Priority: MEDIUM-HIGH)

Text processing is a major application domain currently underrepresented in the suite.

#### 2.1 Core Text Algorithms

**Word Counts** (`word-counts.rkt`)
- **Description:** Count occurrences of each word in text
- **Input:** Text string
- **Output:** Hash table or list of (word, count) pairs
- **Parallelization:** Parallel tokenization + parallel reduction
- **Validation:** Verify counts against sequential version
- **Estimated effort:** 2-3 days
- **Value:** Fundamental text processing operation, tests hash table parallelism

**Inverted Index** (`inverted-index.rkt`)
- **Description:** Build inverted index (word → document list)
- **Input:** Collection of documents
- **Output:** Map from words to document IDs
- **Parallelization:** Parallel document processing + parallel merge
- **Validation:** Verify index completeness and correctness
- **Estimated effort:** 3-4 days
- **Value:** Important for search/IR applications

**Tokens** (`tokens.rkt`)
- **Description:** Tokenize text into words/tokens
- **Input:** Text string, delimiter rules
- **Output:** Sequence of tokens
- **Parallelization:** Parallel scanning with prefix sums
- **Validation:** Compare with sequential tokenization
- **Estimated effort:** 2 days
- **Value:** Building block for text processing, tests irregular parallelism

#### 2.2 Advanced Text Algorithms

**Burrows-Wheeler Decode** (`bw-decode.rkt`)
- **Description:** Decode BWT-encoded string
- **Input:** BWT-encoded string
- **Output:** Original string
- **Parallelization:** Parallel inverse transformation
- **Validation:** Encode then decode returns original
- **Estimated effort:** 4-5 days
- **Value:** Advanced text algorithm, used in compression (bzip2)

**Longest Repeated Substring** (`lrs.rkt`)
- **Description:** Find longest substring that appears at least twice
- **Input:** Text string
- **Output:** Longest repeated substring
- **Parallelization:** Build suffix array, parallel LCP array, parallel max
- **Validation:** Verify substring appears twice, check maximality
- **Estimated effort:** 3-4 days
- **Value:** Tests suffix array usage, important string algorithm

### Phase 3: Computational Geometry (Priority: MEDIUM)

Geometry algorithms complement the existing convex-hull benchmark.

#### 3.1 Core Geometry Algorithms

**Delaunay Triangulation** (`delaunay-triangulation.rkt`)
- **Description:** Compute Delaunay triangulation of 2D points
- **Input:** Set of 2D points
- **Output:** Triangle mesh (list of triangles)
- **Parallelization:** Divide-and-conquer or incremental with parallelization
- **Validation:** Verify Delaunay property (empty circumcircle)
- **Estimated effort:** 6-8 days
- **Value:** Complex geometry algorithm, foundational for graphics/GIS

**K-Nearest Neighbors** (`knn.rkt`)
- **Description:** Find k nearest neighbors for each point
- **Input:** Point set, k value
- **Output:** For each point, list of k nearest neighbors
- **Parallelization:** Parallel spatial data structure (k-d tree) + parallel queries
- **Validation:** Verify distances, compare with brute force
- **Estimated effort:** 5-6 days
- **Value:** Important spatial query, tests tree-based parallelism

**Ray Cast** (`ray-cast.rkt`)
- **Description:** Ray-triangle intersection testing
- **Input:** Set of rays, set of triangles
- **Output:** For each ray, first intersected triangle (if any)
- **Parallelization:** Parallel spatial acceleration structure + parallel queries
- **Validation:** Verify intersection distances
- **Estimated effort:** 5-6 days
- **Value:** Graphics/rendering algorithm, tests spatial queries

#### 3.2 Advanced Geometry

**Delaunay Refine** (`delaunay-refine.rkt`)
- **Description:** Refine Delaunay triangulation (quality improvement)
- **Input:** Delaunay triangulation, angle threshold
- **Output:** Refined triangulation (no small angles)
- **Parallelization:** Parallel Delaunay refinement algorithm
- **Validation:** Verify all angles above threshold
- **Estimated effort:** 6-7 days
- **Value:** Advanced geometry, mesh generation for FEM
- **Note:** Requires Delaunay triangulation first

**Range Query 2D** (`range-query-2d.rkt`)
- **Description:** Count points in rectangles
- **Input:** Point set, set of query rectangles
- **Output:** Count of points in each rectangle
- **Parallelization:** Parallel spatial data structure + parallel queries
- **Validation:** Verify counts against brute force
- **Estimated effort:** 4-5 days
- **Value:** Spatial database operation, tests range trees

### Phase 4: Additional Algorithms (Priority: MEDIUM-LOW)

#### 4.1 Numerical & Scientific

**Dense Matrix Multiplication** (`dmm.rkt`)
- **Description:** Dense matrix multiplication C = A × B
- **Input:** Two matrices
- **Output:** Product matrix
- **Parallelization:** Parallel loops over output elements, tiling
- **Validation:** Compare with sequential multiplication
- **Estimated effort:** 2-3 days
- **Value:** Fundamental linear algebra operation
- **Note:** Overlap with Sandmark plan - coordinate to avoid duplication

**Primes** (`primes.rkt`)
- **Description:** Generate prime numbers up to N (Sieve of Eratosthenes)
- **Input:** Upper bound N
- **Output:** List of primes ≤ N
- **Parallelization:** Parallel segmented sieve
- **Validation:** Verify all outputs are prime, no primes missing
- **Estimated effort:** 3-4 days
- **Value:** Classic parallel algorithm, tests bit vector operations

#### 4.2 Machine Learning

**Classify** (`classify.rkt`)
- **Description:** k-NN classification of feature vectors
- **Input:** Training set (vectors with labels), test set
- **Output:** Predicted labels for test set
- **Parallelization:** Parallel k-NN search + parallel voting
- **Validation:** Verify accuracy on known datasets
- **Estimated effort:** 4-5 days
- **Value:** Machine learning application, tests parallel k-NN

## Implementation Guidelines

### Directory Structure

All new benchmarks go into the existing `benchmarks/mpl/` directory:

```
benchmarks/
  mpl/
    README.md                      # Update with new benchmarks
    # Existing (7)
    histogram.rkt
    integer-sort.rkt
    bfs.rkt
    mis.rkt
    msf.rkt
    suffix-array.rkt
    convex-hull.rkt
    # Phase 1 - Sequence & Graph (4)
    comparison-sort.rkt
    remove-duplicates.rkt
    maximal-matching.rkt
    spanning-forest.rkt
    # Phase 2 - Text Processing (5)
    word-counts.rkt
    inverted-index.rkt
    tokens.rkt
    bw-decode.rkt
    lrs.rkt
    # Phase 3 - Geometry (5)
    delaunay-triangulation.rkt
    knn.rkt
    ray-cast.rkt
    delaunay-refine.rkt
    range-query-2d.rkt
    # Phase 4 - Additional (3)
    dmm.rkt
    primes.rkt
    classify.rkt
tests/
  mpl/
    # Add tests for each new benchmark
    comparison-sort-test.rkt
    ...
```

### Common Patterns

All MPL benchmarks should follow the established patterns from existing benchmarks:

#### 1. Standard Structure

```racket
#lang racket
(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

;; Sequential implementation
(define (benchmark-sequential input)
  ...)

;; Parallel implementation
(define (benchmark-parallel input workers)
  ...)

;; Main entry point
(module+ main
  (define args (parse-args))
  (run-benchmark args))
```

#### 2. CLI Arguments

```racket
(define (parse-args)
  (command-line
   #:program "benchmark-name"
   #:once-each
   [("--n") n "Problem size" (set! n (string->number n))]
   [("--workers") w "Worker count" (set! workers (string->number w))]
   [("--repeat") r "Repetitions" (set! repeats (string->number r))]
   [("--input") file "Input file" (set! input-file file)]
   [("--seed") s "Random seed" (set! seed (string->number s))]
   [("--log") file "Log file" (set! log-file file)]))
```

#### 3. Verification

```racket
(define (verify-correctness seq-result par-result)
  (cond
    [(not (equal? seq-result par-result))
     (error 'benchmark "Results differ: seq=~a par=~a" seq-result par-result)]
    [else (printf "Verification passed\n")]))
```

#### 4. Attribution

Include clear attribution to MPL/PBBS:

```racket
;; Re-implementation of [benchmark-name] from MPL parallel-ml-bench
;; Based on PBBS benchmark: [PBBS-URL]
;; Original MPL: https://github.com/MPLLang/parallel-ml-bench
;; Adapted for Racket parallel benchmarking
```

### Racket-Specific Considerations

#### Parallelism Primitives

Use the existing infrastructure from `benchmarks/common/parallel.rkt`:

- **Thread pools** - For coarse-grained task parallelism
- **Futures** - For fine-grained data parallelism
- **Parallel loops** - Custom `for/parallel` macro

#### Data Structures

- **Vectors** - For arrays and fixed-size sequences
- **Lists** - For variable-length results
- **Hash tables** - For dictionaries and mappings
- **Structs** - For complex data (points, edges, triangles)

#### Performance Tips

1. **Minimize allocation** in hot loops
2. **Use typed Racket** for critical paths (optional)
3. **Chunk work** appropriately to balance overhead
4. **Profile** before optimizing

### Testing Strategy

For each benchmark, provide:

1. **Correctness tests**
   - Small inputs with known outputs
   - Sequential vs parallel equivalence
   - Property-based checks where applicable

2. **Smoke tests**
   - Fast execution (< 1s)
   - Suitable for CI

3. **Scaling tests**
   - Verify speedup with increased workers
   - Document expected scaling characteristics

4. **Input generators**
   - Random input generation with seeds
   - Standard datasets where available

### Integration with Suite

Update `benchmarks/run-suite.rkt` to include new benchmarks:

```racket
(define mpl-benchmarks
  '(;; Existing
    (histogram (n 10000000) (buckets 256) (workers 1 2 4 8))
    ...
    ;; New
    (comparison-sort (n 10000000) (workers 1 2 4 8))
    (word-counts (n 1000000) (workers 1 2 4 8))
    (delaunay-triangulation (n 10000) (workers 1 2 4 8))
    ...))
```

Update config files:
- `benchmarks/config/mpl-quick.sexp` - Small problem sizes
- `benchmarks/config/mpl-standard.sexp` - Medium problem sizes
- `benchmarks/config/mpl-stress.sexp` - Large problem sizes

## Priority and Timeline

### High Priority (Phase 1 & 2) - Target: 6-8 weeks

**Week 1-2:**
- Comparison sort
- Remove duplicates
- Maximal matching

**Week 3-4:**
- Word counts
- Tokens
- Inverted index

**Week 5-6:**
- Longest repeated substring
- Burrows-Wheeler decode

**Week 7-8:**
- Testing, documentation, integration
- Spanning forest (if time permits)

**Deliverables:**
- 7-8 new benchmarks in `benchmarks/mpl/`
- Comprehensive tests
- Updated README with usage examples
- Integration with suite runner

### Medium Priority (Phase 3) - Target: 6-8 weeks

**Week 1-3:**
- K-nearest neighbors
- Ray cast

**Week 4-6:**
- Delaunay triangulation (complex)

**Week 7-8:**
- Range query 2D
- Delaunay refine (if time permits)

**Deliverables:**
- 3-4 geometry benchmarks
- Spatial data structure implementations
- Visualization outputs (optional)

### Low Priority (Phase 4) - Target: 3-4 weeks

**Week 1-2:**
- Dense matrix multiply
- Primes

**Week 3-4:**
- Classify
- Final testing and documentation

**Deliverables:**
- 3 additional benchmarks
- Complete MPL suite coverage

## Technical Challenges

### 1. Spatial Data Structures

**Challenge:** Implementing efficient parallel spatial data structures (k-d trees, quadtrees, BVH).

**Solutions:**
- Start with simple structures, optimize later
- Consider using existing Racket libraries where available
- Focus on correctness first, performance second

### 2. Complex Geometry Algorithms

**Challenge:** Delaunay triangulation is algorithmically complex.

**Solutions:**
- Start with divide-and-conquer approach
- Use incremental algorithm with parallel optimization
- Extensive testing with known triangulations

### 3. Text Processing at Scale

**Challenge:** Large text inputs may stress memory and GC.

**Solutions:**
- Use chunked processing
- Minimize string allocation
- Consider byte-vector operations for performance

### 4. Irregular Parallelism

**Challenge:** Many benchmarks have irregular or data-dependent parallelism.

**Solutions:**
- Use work-stealing thread pools
- Implement granularity thresholds
- Profile and tune chunk sizes

## Coordination with Sandmark Plan

### Overlapping Benchmarks

Some benchmarks appear in both plans:

1. **Dense Matrix Multiplication (dmm)**
   - **Decision:** Implement in `benchmarks/sandmark/matrix-mult.rkt`
   - **Reason:** More natural fit with Sandmark numerical algorithms
   - **Action:** Reference from MPL README, don't duplicate

2. **Comparison Sort**
   - **MPL:** General parallel sort (merge sort)
   - **Sandmark:** Specific algorithms (merge sort, quicksort)
   - **Decision:** Implement both - MPL version as general sort, Sandmark versions as specific algorithms
   - **Action:** Coordinate naming and placement

3. **Primes (Sieve)**
   - **Decision:** Implement in `benchmarks/mpl/primes.rkt`
   - **Reason:** Better fit with sequence algorithms in MPL

### Coordination Strategy

1. **Before implementing overlapping benchmarks:**
   - Check if already implemented in Sandmark
   - Decide which suite is the better fit
   - Document decision in both plans

2. **Naming conventions:**
   - Use descriptive names that indicate provenance
   - Example: `mpl-sort.rkt` vs `sandmark-merge-sort.rkt`

3. **Cross-reference:**
   - Each suite's README should reference related benchmarks in other suites

## Success Metrics

### Quantitative
- **15-18 new benchmarks** in `benchmarks/mpl/`
- **Total MPL suite: 22-25 benchmarks** (7 existing + 15-18 new)
- **100% test coverage** for correctness
- **Documentation** for all benchmarks

### Qualitative
- Comprehensive coverage of PBBS benchmark categories
- Clear attribution to MPL/PBBS
- Consistent with existing benchmark architecture
- Useful for Racket parallel performance evaluation
- Good balance of simple and complex algorithms

## Exclusions and Deferred Items

### Not Implementing

1. **Spanning Forest** - May be redundant with MSF (unweighted version)
   - **Reason:** Limited additional value beyond MSF
   - **Alternative:** Add as optional extra if time permits

2. **Audio Processing Benchmarks** - Specifics unclear from MPL documentation
   - **Reason:** Insufficient information to design proper ports
   - **Alternative:** Research further if specific benchmarks identified

### Deferred for Future

1. **3D Geometry Algorithms** - k-NN in 3D, etc.
   - **Reason:** 2D versions provide sufficient coverage initially
   - **Future:** Add 3D variants in later iterations

2. **Advanced Graph Algorithms** - Graph coloring, max flow, etc.
   - **Reason:** Not in core PBBS/MPL set
   - **Future:** Could be added as extensions

## References

- [MPL parallel-ml-bench](https://github.com/MPLLang/parallel-ml-bench)
- [PBBS V2 Benchmark Suite](https://cmuparlay.github.io/pbbsbench/)
- [ParlayLib](https://cmuparlay.github.io/parlaylib/)
- [MPL Compiler](https://github.com/MPLLang/mpl)
- [POPL 2021: Provably Space-Efficient Parallel Functional Programming](https://dl.acm.org/doi/10.1145/3434299)
- [ICFP 2022: Entanglement Detection with Near-Zero Cost](https://dl.acm.org/doi/10.1145/3547646)

## Appendix: Complete PBBS/MPL Inventory Status

### Sequences (4 total, 2 done, 2 to port)
- ✓ histogram - **DONE**
- ✓ integer-sort - **DONE**
- ☐ comparison-sort - **PORT** (Phase 1)
- ☐ remove-duplicates - **PORT** (Phase 1)

### Graphs (5 total, 3 done, 2 to port)
- ✓ breadth-first-search - **DONE**
- ✓ maximal-independent-set - **DONE**
- ✓ min-spanning-forest - **DONE**
- ☐ maximal-matching - **PORT** (Phase 1)
- ☐ spanning-forest - **PORT** (Phase 1, optional)

### Text Processing (5 total, 1 done, 4 to port)
- ✓ suffix-array - **DONE**
- ☐ word-counts - **PORT** (Phase 2)
- ☐ inverted-index - **PORT** (Phase 2)
- ☐ bw-decode - **PORT** (Phase 2)
- ☐ longest-repeated-substring - **PORT** (Phase 2)

### Geometry (6 total, 1 done, 5 to port)
- ✓ convex-hull - **DONE**
- ☐ delaunay-triangulation - **PORT** (Phase 3)
- ☐ nearest-neighbors - **PORT** (Phase 3)
- ☐ ray-cast - **PORT** (Phase 3)
- ☐ delaunay-refine - **PORT** (Phase 3)
- ☐ range-query-2d - **PORT** (Phase 3)

### Other (2 total, 1 done, 1 to port)
- ✗ nbody - **DONE** (in shootout, not MPL)
- ☐ classify - **PORT** (Phase 4)

### MPL-Specific (3-5, 0 done, 3 to port)
- ☐ tokens - **PORT** (Phase 2)
- ☐ primes - **PORT** (Phase 4)
- ☐ dmm - **DEFER** (coordinate with Sandmark)

**Summary:**
- **Total benchmarks:** 22-24 (PBBS) + 3-5 (MPL-specific) = ~25-29
- **Already done:** 9 (7 MPL + 2 elsewhere)
- **To port:** 15-18 benchmarks
- **Excluding:** spanning-forest (optional), dmm (Sandmark)

---

## Next Steps

1. **VERIFY MPL BENCHMARK LIST** - Attempt to access the actual MPL parallel-ml-bench repository to confirm which benchmarks are actually implemented
   - Options: Clone the repo directly, contact MPL maintainers, find recent papers with complete benchmark lists
   - This is critical before committing significant implementation effort

2. **Review this plan** with project stakeholders, acknowledging PBBS-based assumptions

3. **Prioritize Phase 1 benchmarks** (comparison-sort, remove-duplicates, maximal-matching) - these are safe bets as they're in PBBS

4. **Begin implementation** with comparison-sort as pilot

5. **Update benchmarks/mpl/README.md** to reflect new benchmarks as they're added

6. **Coordinate with Sandmark plan** for overlapping benchmarks (dmm, sorting)

7. **Set up test infrastructure** for new benchmark categories
