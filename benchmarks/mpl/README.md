# MPL Parallel ML Benchmarks - Racket Re-implementations

This directory contains Racket re-implementations of selected benchmarks from the MPL (MaPLe) Parallel ML Benchmark suite. These benchmarks are originally based on the Problem-Based Benchmark Suite (PBBS) and ParlayLib, which provide a comprehensive collection of parallel algorithms across various domains.

**Important:** These are **Racket re-implementations** of the algorithms, not wrappers around the MPL compiler. The goal is to compare Racket's parallel programming capabilities (using futures and places) against the reference MPL implementations.

## About the MPL Benchmark Suite

The MPL parallel-ml-bench suite consists of sophisticated parallel benchmarks ported from state-of-the-art C++ parallel benchmark suites (PBBS, ParlayLib, Ligra, PAM) to the MPL language. These benchmarks cover:

- **Graph algorithms**: BFS, MIS, MSF, maximal matching, spanning forest
- **Sequence algorithms**: Sorting, histogram, duplicate removal
- **Text processing**: Suffix array, word counts, inverted index, Burrows-Wheeler
- **Computational geometry**: Convex hull, Delaunay triangulation, ray casting, nearest neighbors
- **Numerical/scientific**: N-body simulation, classification

## Implemented Benchmarks

### Histogram (histogram.rkt)
**Problem:** Given a sequence of integers in range [0, m), count the occurrences of each value.

**Input:** Sequence of n integers, bucket count m
**Output:** Array of m counts
**Complexity:** O(n) work, O(log n) span

**Variants:**
- Sequential: Simple array accumulation
- Parallel: Partitioned counting with reduction

**Parameters:**
- `--n <size>`: Number of elements (default: 10000000)
- `--buckets <m>`: Number of histogram buckets (default: 256)
- `--workers <count>`: Parallel worker threads
- `--repeat <n>`: Benchmark repetitions

### Integer Sort (integer-sort.rkt)
**Problem:** Sort a sequence of integers (counting sort / radix sort approach).

**Input:** Sequence of n integers
**Output:** Sorted sequence
**Complexity:** O(n) work for bounded range, O(log n) span

**Variants:**
- Sequential: Counting sort for bounded ranges
- Parallel: Parallel counting sort with partitioned reduction

**Parameters:**
- `--n <size>`: Number of elements
- `--range <max>`: Maximum value (for bounded sort)
- `--workers <count>`: Parallel worker threads

### Breadth-First Search - BFS (bfs.rkt)
**Problem:** Compute a BFS tree from a given source vertex in an undirected graph.

**Input:** Graph (adjacency list), source vertex
**Output:** Parent array representing BFS tree
**Complexity:** O(|V| + |E|) work, O(diameter) span

**Variants:**
- Sequential: Queue-based BFS
- Parallel: Level-synchronous parallel BFS

**Parameters:**
- `--graph <file>`: Graph input file (edge list format)
- `--source <v>`: Source vertex (default: 0)
- `--workers <count>`: Parallel worker threads

### Maximal Independent Set - MIS (mis.rkt)
**Problem:** Find a maximal independent set in an undirected graph (a set of vertices where no two are adjacent, and no vertex can be added while maintaining this property).

**Input:** Graph (adjacency list)
**Output:** List of vertex indices in the MIS
**Complexity:** O(|V| + |E|) work expected, O(log |V|) span expected (randomized)

**Variants:**
- Sequential: Greedy algorithm
- Parallel: Luby's randomized parallel algorithm

**Parameters:**
- `--n <size>`: Number of vertices (for random graph generation)
- `--degree <avg>`: Average degree (for random graph generation)
- `--graph <file>`: Graph input file (edge list format)
- `--workers <count>`: Parallel worker threads
- `--seed <n>`: Random seed

### Minimum Spanning Forest - MSF (msf.rkt)
**Problem:** Find a minimum spanning forest of a weighted undirected graph (minimum spanning tree for each connected component).

**Input:** Weighted graph (adjacency list with weights)
**Output:** List of edges in the MSF
**Complexity:** O(|E| log |V|) work, O(log² |V|) span expected

**Variants:**
- Sequential: Kruskal's algorithm with union-find
- Parallel: Borůvka's algorithm

**Parameters:**
- `--n <size>`: Number of vertices (for random graph generation)
- `--degree <avg>`: Average degree (for random graph generation)
- `--graph <file>`: Weighted graph input file (format: u v weight)
- `--workers <count>`: Parallel worker threads
- `--seed <n>`: Random seed

### Suffix Array (suffix-array.rkt)
**Problem:** Construct a suffix array of a text string (lexicographically sorted array of all suffixes).

**Input:** Text string
**Output:** Suffix array (vector of starting positions)
**Complexity:** O(n log n) work, O(log² n) span

**Variants:**
- Sequential: Prefix-doubling algorithm
- Parallel: Parallel prefix-doubling with parallel sorting

**Parameters:**
- `--n <size>`: Text length (for random generation)
- `--alphabet <size>`: Alphabet size (for random generation)
- `--text <file>`: Text input file
- `--workers <count>`: Parallel worker threads
- `--seed <n>`: Random seed

### Convex Hull (convex-hull.rkt)
**Problem:** Find the convex hull of a set of 2D points (smallest convex polygon containing all points).

**Input:** List of 2D points
**Output:** List of hull vertices in counter-clockwise order
**Complexity:** O(n log n) work expected, O(log² n) span expected

**Variants:**
- Sequential: QuickHull algorithm
- Parallel: Parallel QuickHull with divide-and-conquer

**Parameters:**
- `--n <size>`: Number of points (for random generation)
- `--distribution <type>`: Point distribution (uniform-circle, uniform-square, circle-perimeter)
- `--points <file>`: Points input file (format: x y per line)
- `--workers <count>`: Parallel worker threads
- `--threshold <size>`: Sequential threshold for parallel algorithm
- `--seed <n>`: Random seed

## Usage

### Running Individual Benchmarks

```bash
# Histogram
racket benchmarks/mpl/histogram.rkt \
  --n 10000000 \
  --buckets 256 \
  --workers 8 \
  --repeat 3 \
  --log logs/mpl-histogram.sexp

# Integer Sort
racket benchmarks/mpl/integer-sort.rkt \
  --n 10000000 \
  --range 1000000 \
  --workers 8 \
  --repeat 3 \
  --log logs/mpl-isort.sexp

# BFS
racket benchmarks/mpl/bfs.rkt \
  --graph inputs/graph.txt \
  --source 0 \
  --workers 8 \
  --repeat 3 \
  --log logs/mpl-bfs.sexp

# MIS
racket benchmarks/mpl/mis.rkt \
  --n 10000 \
  --degree 10 \
  --workers 8 \
  --repeat 3 \
  --log logs/mpl-mis.sexp

# MSF
racket benchmarks/mpl/msf.rkt \
  --n 1000 \
  --degree 10 \
  --workers 8 \
  --repeat 3 \
  --log logs/mpl-msf.sexp

# Suffix Array
racket benchmarks/mpl/suffix-array.rkt \
  --n 100000 \
  --alphabet 4 \
  --workers 8 \
  --repeat 3 \
  --log logs/mpl-suffix-array.sexp

# Convex Hull
racket benchmarks/mpl/convex-hull.rkt \
  --n 10000 \
  --distribution uniform-circle \
  --workers 8 \
  --repeat 3 \
  --log logs/mpl-convex-hull.sexp
```

### Running via Suite Runner

```bash
racket benchmarks/run-suite.rkt --suite mpl --config config/quick.sexp
```

## Implementation Notes

### Parallelization Strategy

All benchmarks provide both sequential and parallel variants:

1. **Sequential baseline**: Straightforward, optimized sequential implementation
2. **Parallel variant**: Uses Racket's parallel constructs:
   - `futures` for fine-grained parallelism
   - `places` for coarse-grained, shared-nothing parallelism
   - Parallel loops via `for/parallel` (custom macro in `common/parallel.rkt`)

### Verification

Each benchmark includes:
- Correctness verification comparing sequential and parallel outputs
- Deterministic test inputs for reproducibility
- Property-based checks where applicable

### Performance Considerations

- **Memory layout**: Use Racket vectors and typed arrays where beneficial
- **Work stealing**: Rely on Racket's built-in futures scheduler
- **Granularity control**: Tunable chunk sizes to balance overhead and parallelism
- **GC pressure**: Minimize allocation in hot paths

## Comparison with MPL

Key differences from the MPL implementations:

1. **Language features**: MPL has specialized parallel primitives (`par`, `parfor`, `reducem`) while Racket uses futures/places
2. **Memory model**: MPL uses provably efficient parallel GC; Racket's GC has different characteristics
3. **Type system**: MPL is statically typed (Standard ML); Racket is dynamically typed (optional Typed Racket)
4. **Compilation**: MPL does whole-program compilation via MLton; Racket uses JIT compilation

## Future Work

Additional benchmarks that could be implemented in future iterations:

- **N-body**: Scientific computing (gravitational or molecular dynamics simulation)
- **Delaunay Triangulation**: Computational geometry
- **Ray Casting**: Graphics and computational geometry
- **K-Nearest Neighbors**: Spatial data structures and search

## References

- [MPL parallel-ml-bench](https://github.com/MPLLang/parallel-ml-bench)
- [PBBS Benchmark Suite V2](https://cmuparlay.github.io/pbbsbench/)
- [ParlayLib](https://cmuparlay.github.io/parlaylib/)
- [MPL Compiler](https://github.com/MPLLang/mpl)
