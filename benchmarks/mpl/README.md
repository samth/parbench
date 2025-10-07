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

Additional benchmarks planned for re-implementation:

- **MIS** (Maximal Independent Set): Graph algorithm
- **MSF** (Minimum Spanning Forest): Weighted graph algorithm
- **Suffix Array**: Text processing
- **Convex Hull**: Computational geometry
- **N-body**: Scientific computing

## References

- [MPL parallel-ml-bench](https://github.com/MPLLang/parallel-ml-bench)
- [PBBS Benchmark Suite V2](https://cmuparlay.github.io/pbbsbench/)
- [ParlayLib](https://cmuparlay.github.io/parlaylib/)
- [MPL Compiler](https://github.com/MPLLang/mpl)
