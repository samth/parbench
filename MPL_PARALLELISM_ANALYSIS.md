# MPL Benchmarks: Parallelism-Unfriendly Data Structure Analysis

**Date:** 2025-12-11
**Author:** Claude Analysis
**Scope:** MPL benchmark suite (`benchmarks/mpl/`)

## Executive Summary

Analysis of 27 MPL benchmarks revealed **significant use of parallelism-unfriendly data structures**, particularly singly-linked lists, in performance-critical paths. The main issues are:

1. **Graph algorithms** (BFS, MIS, MSF) use adjacency **lists** instead of vectors
2. **Sorting algorithms** (samplesort, quicksort partitioning) build intermediate results as lists
3. **Geometric algorithms** (convex-hull) operate entirely on lists
4. **Deduplication** uses O(n²) vector-append pattern
5. **Multiple benchmarks** convert vectors↔lists unnecessarily during sorting

These issues harm parallel performance due to:
- Poor cache locality (lists scatter data across memory)
- Sequential allocation patterns (cons cells allocated one-by-one)
- Expensive traversal (pointer chasing instead of sequential access)
- Memory overhead (64-bit pointers per element vs. contiguous vectors)

## Critical Issues by Benchmark

### 🔴 CRITICAL: BFS (bfs.rkt)

**Problem:** Graph adjacency representation uses singly-linked lists

**Location:**
- Lines 16-26: Graph construction with lists
- Lines 41-44, 69, 91-93: Neighbor traversal in hot paths

**Impact:**
- BFS is memory-bandwidth intensive
- Lists cause cache misses on every neighbor access
- Parallel frontier construction builds lists inefficiently

**Current Code:**
```racket
(define (make-graph n edges)
  (define adj (make-vector n '()))  ; Vector of LISTS ❌
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    (vector-set! adj u (cons v (vector-ref adj u)))  ; cons builds lists
    (vector-set! adj v (cons u (vector-ref adj v))))
  (graph n adj))
```

**Proposed Fix:**
```racket
(define (make-graph n edges)
  ;; First pass: count degree of each vertex
  (define degrees (make-vector n 0))
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    (vector-set! degrees u (+ 1 (vector-ref degrees u)))
    (vector-set! degrees v (+ 1 (vector-ref degrees v))))

  ;; Second pass: allocate neighbor vectors
  (define adj (for/vector ([d (in-vector degrees)])
                (make-vector d)))

  ;; Third pass: fill neighbor vectors
  (define positions (make-vector n 0))
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    (define u-pos (vector-ref positions u))
    (define v-pos (vector-ref positions v))
    (vector-set! (vector-ref adj u) u-pos v)
    (vector-set! (vector-ref adj v) v-pos u)
    (vector-set! positions u (+ u-pos 1))
    (vector-set! positions v (+ v-pos 1)))

  (graph n adj))
```

**Benefits:**
- Contiguous memory for neighbors → better cache utilization
- Random access to any neighbor in O(1)
- Better parallel scaling due to reduced memory traffic

**Estimated Speedup:** 1.5-2x on large graphs with high average degree

---

### 🔴 CRITICAL: MIS (mis.rkt)

**Problem:** Graph construction builds lists then converts to vectors

**Location:**
- Lines 36-46: `read-graph-edgelist` builds adjacency lists
- Lines 167-182: `generate-random-graph` builds adjacency lists

**Current Code:**
```racket
(define adj-lists (make-vector n null))
(for ([edge (in-list edge-pairs)])
  (define u (first edge))
  (define v (second edge))
  (vector-set! adj-lists u (cons v (vector-ref adj-lists u)))  ; ❌ cons
  (vector-set! adj-lists v (cons u (vector-ref adj-lists v))))

;; Convert to vector of vectors
(for/vector ([neighbors (in-vector adj-lists)])
  (list->vector neighbors))  ; Wasteful conversion
```

**Proposed Fix:**
Use the same two-pass approach as BFS above:
1. Count degrees
2. Allocate exact-size vectors
3. Fill vectors directly

**Benefits:**
- Eliminates list→vector conversion overhead
- Reduces memory allocations from O(edges) to O(vertices)
- Better memory locality during graph processing

**Estimated Speedup:** 1.2-1.5x (less critical since conversion happens once)

---

### 🔴 CRITICAL: MSF (msf.rkt)

**Problem:** Multiple uses of lists in hot paths

**Location:**
- Lines 37-48: Graph construction via lists
- Lines 88-98: Edge collection via nested lists with `apply append`
- Lines 105, 113: MSF edges accumulated as list
- Lines 207-218: Random graph generation via lists

**Current Code (Edge Collection):**
```racket
(define edges
  (apply append  ; ❌ Expensive append
         (for/list ([u (in-range n)])
           (for/list ([neighbor-weight (in-vector (vector-ref graph u))])
             (define v (car neighbor-weight))
             (define w (cdr neighbor-weight))
             (if (< u v)
                 (list u v w)
                 #f)))))
(define edges-filtered (filter identity edges))
```

**Proposed Fix:**
```racket
;; Pre-count edges
(define edge-count
  (for/sum ([u (in-range n)])
    (for/sum ([neighbor-weight (in-vector (vector-ref graph u))])
      (if (< u (car neighbor-weight)) 1 0))))

;; Allocate result vector
(define edges (make-vector edge-count))
(define pos 0)
(for ([u (in-range n)])
  (for ([neighbor-weight (in-vector (vector-ref graph u))])
    (define v (car neighbor-weight))
    (define w (cdr neighbor-weight))
    (when (< u v)
      (vector-set! edges pos (vector u v w))
      (set! pos (+ pos 1)))))
```

**For MSF edges accumulation:**
```racket
;; Pre-allocate maximum possible edges (n-1 for spanning tree)
(define msf-edges (make-vector (- n 1)))
(define msf-count 0)

(for ([edge (in-list sorted-edges)])
  (define u (first edge))
  (define v (second edge))
  (define w (third edge))
  (unless (= (uf-find! uf u) (uf-find! uf v))
    (uf-union! uf u v)
    (vector-set! msf-edges msf-count edge)
    (set! msf-count (+ msf-count 1))))

;; Trim to actual size
(vector-copy msf-edges 0 msf-count)
```

**Benefits:**
- Eliminates expensive `apply append` on large edge lists
- Reduces allocations
- Better cache behavior

**Estimated Speedup:** 1.3-1.8x on large graphs

---

### 🔴 CRITICAL: Convex Hull (convex-hull.rkt)

**Problem:** Entire algorithm operates on lists

**Location:**
- Lines 22-27: `read-points` returns list
- Lines 52-83: QuickHull recursively builds/filters lists
- Lines 175-194: Point generation returns list

**Current Code:**
```racket
(define (quickhull-sequential points a b)
  (if (null? points)
      null
      (let ([farthest (find-farthest-point points a b)])
        (if (not farthest)
            null
            (let ([left-set (filter (λ (p) ...) points)]   ; ❌ filter on list
                  [right-set (filter (λ (p) ...) points)])
              (append (quickhull-sequential left-set a farthest)  ; ❌ append
                      (list farthest)
                      (quickhull-sequential right-set farthest b)))))))
```

**Proposed Fix:**
Rewrite to use vectors throughout:

```racket
;; Read points as vector
(define (read-points path)
  (define lines (file->lines path))
  (for/vector ([line (in-list lines)])
    (define parts (string-split line))
    (point (string->number (first parts))
           (string->number (second parts)))))

;; Vector-based filter
(define (vector-filter pred vec)
  (define matches
    (for/list ([x (in-vector vec)] #:when (pred x)) x))
  (list->vector matches))

;; QuickHull with vectors
(define (quickhull-sequential points a b)
  (define n (vector-length points))
  (if (= n 0)
      (vector)
      (let ([farthest (find-farthest-point points a b)])
        (if (not farthest)
            (vector)
            (let ([left-set (vector-filter (λ (p) (> (cross-product a farthest p) 0)) points)]
                  [right-set (vector-filter (λ (p) (> (cross-product farthest b p) 0)) points)])
              (vector-append
                (quickhull-sequential left-set a farthest)
                (vector farthest)
                (quickhull-sequential right-set farthest b)))))))

;; Generate as vector
(define (generate-random-points n seed [distribution 'uniform-circle])
  (random-seed seed)
  (for/vector ([i (in-range n)])  ; ✅ for/vector instead of for/list
    ...))
```

**Benefits:**
- Eliminates pointer-chasing through point lists
- Better memory locality for geometric predicates
- Faster filtering (sequential scan vs pointer chasing)

**Estimated Speedup:** 2-3x (lists are particularly bad for numeric computation)

---

### 🔴 CRITICAL: Samplesort (samplesort.rkt)

**Problem:** Bucket accumulation uses lists

**Location:**
- Lines 38-44: QuickSort partitioning builds lists
- Lines 73-82: Bucket partitioning via cons

**Current Code:**
```racket
(define buckets (make-vector workers '()))
(for ([elem (in-vector vec)])
  (define bucket-idx ...)
  (vector-set! buckets bucket-idx
               (cons elem (vector-ref buckets bucket-idx))))  ; ❌ cons
```

**Proposed Fix:**
```racket
;; Two-pass bucketing
;; Pass 1: Count bucket sizes
(define bucket-counts (make-vector workers 0))
(for ([elem (in-vector vec)])
  (define bucket-idx ...)
  (vector-set! bucket-counts bucket-idx
               (+ 1 (vector-ref bucket-counts bucket-idx))))

;; Allocate bucket vectors
(define buckets
  (for/vector ([count (in-vector bucket-counts)])
    (make-vector count)))

;; Pass 2: Fill buckets
(define bucket-positions (make-vector workers 0))
(for ([elem (in-vector vec)])
  (define bucket-idx ...)
  (define pos (vector-ref bucket-positions bucket-idx))
  (define bucket (vector-ref buckets bucket-idx))
  (vector-set! bucket pos elem)
  (vector-set! bucket-positions bucket-idx (+ pos 1)))
```

**Benefits:**
- Eliminates O(n) cons operations
- Better cache locality when sorting buckets
- Predictable memory layout

**Estimated Speedup:** 1.5-2x

---

### 🔴 CRITICAL: Dedup (dedup.rkt)

**Problem:** O(n²) vector-append in sequential; lists in parallel

**Location:**
- Lines 31-36: Sequential dedup appends one element at a time
- Lines 44-58: Parallel bucketing via cons

**Current Code (Sequential):**
```racket
(define (dedup-sequential data)
  (define seen (make-hash))
  (define result (make-vector 0))
  (for ([elem (in-vector data)])
    (unless (hash-has-key? seen elem)
      (hash-set! seen elem #t)
      (set! result (vector-append result (vector elem)))))  ; ❌ O(n²)
  result)
```

**Proposed Fix:**
```racket
(define (dedup-sequential data)
  (define seen (make-hash))
  (define result (make-vector (vector-length data)))  ; Max possible size
  (define count 0)
  (for ([elem (in-vector data)])
    (unless (hash-has-key? seen elem)
      (hash-set! seen elem #t)
      (vector-set! result count elem)
      (set! count (+ count 1))))
  (vector-copy result 0 count))  ; Trim to actual size
```

**For parallel version:**
Apply the two-pass bucketing approach from samplesort.

**Benefits:**
- Sequential: O(n²) → O(n)
- Parallel: Eliminates list allocation overhead
- Massive speedup on large datasets

**Estimated Speedup:** 10-100x for sequential (seriously bad right now)

---

### 🟡 MODERATE: Suffix Array (suffix-array.rkt)

**Problem:** Vector↔list conversions during sorting

**Location:**
- Line 19: String→list→vector
- Lines 33-44, 108-118: Vector→list→sort→vector

**Current Code:**
```racket
(define text-vec (list->vector (string->list text)))  ; ❌ Extra conversion

(define sa-list (vector->list sa))
(define sorted-sa (sort sa-list ...))  ; ❌ sort requires list
(set! sa (list->vector sorted-sa))
```

**Proposed Fix:**
```racket
;; Use vector-sort! or implement in-place sort
(define text-vec
  (for/vector ([i (in-range (string-length text))])
    (string-ref text i)))

;; Use vector-sort or vector-sort! (Racket built-in)
(set! sa (vector-sort sa (λ (i j) ...)))
```

**Benefits:**
- Eliminates redundant conversions
- Faster sorting (vectors are more cache-friendly)

**Estimated Speedup:** 1.1-1.3x (not critical since sorting is small fraction of work)

---

## Summary by Severity

### 🔴 CRITICAL (require immediate fixes):
1. **bfs.rkt** - Adjacency lists in graph traversal
2. **mis.rkt** - List-based graph construction
3. **msf.rkt** - Lists in edge collection and MSF building
4. **convex-hull.rkt** - Entire algorithm on lists
5. **samplesort.rkt** - List-based bucketing
6. **dedup.rkt** - O(n²) vector-append + list bucketing

### 🟡 MODERATE (should fix):
7. **suffix-array.rkt** - Unnecessary vector↔list conversions

### 🟢 GOOD (no changes needed):
- **histogram.rkt** ✅
- **integer-sort.rkt** ✅
- **merge-sort.rkt** ✅
- **flatten.rkt** ✅

## Implementation Priority

**Phase 1 (High Impact):**
1. **dedup.rkt** - Easiest fix, biggest speedup (10-100x potential)
2. **bfs.rkt** - Core graph algorithm, widely used
3. **samplesort.rkt** - Important parallel pattern

**Phase 2 (Medium Impact):**
4. **convex-hull.rkt** - Geometric algorithms showcase
5. **msf.rkt** - Classic graph algorithm
6. **mis.rkt** - Less critical (conversion happens once)

**Phase 3 (Polish):**
7. **suffix-array.rkt** - Minor optimization

## Testing Strategy

For each fix:
1. ✅ Run existing tests to ensure correctness
2. ✅ Benchmark before/after with various input sizes
3. ✅ Verify parallel speedup improves
4. ✅ Check memory usage (should decrease)
5. ✅ Profile cache miss rates (should improve)

## References

- **MPL Benchmark Suite:** https://github.com/MPLLang/parallel-ml-bench
- **Racket Vectors:** https://docs.racket-lang.org/reference/vectors.html
- **Why Lists Hurt Parallelism:**
  - Cache line bouncing during cons
  - Poor spatial locality
  - Sequential allocation dependencies
  - Higher memory overhead (2x for pointer + value)

## Notes

- Some benchmarks intentionally match MPL's functional style, but performance should take priority
- Racket's `vector-append` is optimized but still less efficient than pre-allocation
- Two-pass algorithms (count-then-fill) are standard practice in systems programming
- These changes will make the benchmarks more realistic for evaluating Racket's parallel performance
