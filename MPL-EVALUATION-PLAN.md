# MPL Speedup Evaluation Plan

This document outlines the methodology for evaluating parallel speedup in Racket benchmarks and comparing results against MPL (Parallel ML) for ground truth validation.

## Objectives

1. **Measure parallel speedup** for all 27 MPL-ported benchmarks in Racket
2. **Compare results against MPL** to validate correctness and performance characteristics
3. **Establish reliable ground truth** for benchmark outputs

## Phase 1: Racket Benchmark Baseline

### 1.1 Sequential Baseline Collection

Run each benchmark with `--workers 1` to establish sequential performance:

```bash
for bench in histogram integer-sort bfs convex-hull mis msf suffix-array \
             primes merge-sort samplesort tokens nqueens dedup word-count \
             fib shuffle grep palindrome parens mcss flatten collect \
             bignum-add subset-sum triangle-count connectivity centrality; do
  racket benchmarks/mpl/$bench.rkt --workers 1 --repeat 10 --log logs/baseline/$bench.sexp
done
```

### 1.2 Parallel Scaling Tests

Test with worker counts: 1, 2, 4, 6, 8 (and optionally 12, 16 on high-core machines):

```bash
racket run-mpl-benchmarks.rkt --log-dir logs/scaling --output scaling-results.html
```

### 1.3 Key Metrics

For each benchmark, collect:
- **Sequential time** (workers=1)
- **Parallel time** for each worker count
- **Speedup** = sequential_time / parallel_time
- **Efficiency** = speedup / num_workers
- **CPU utilization** = cpu_ms / real_ms (>1 indicates true parallelism)

## Phase 2: MPL Ground Truth Comparison

### 2.1 Obtain MPL Reference Implementation

Clone and build MPL benchmarks:

```bash
git clone https://github.com/MPLLang/parallel-ml-bench.git
cd parallel-ml-bench
# Follow MPL build instructions
```

### 2.2 Benchmark Correspondence

| Racket Benchmark | MPL Benchmark | Notes |
|------------------|---------------|-------|
| histogram | mpl/bench/histogram | Direct port |
| integer-sort | mpl/bench/integersort | Counting sort |
| bfs | mpl/bench/bfs | Level-synchronous BFS |
| convex-hull | mpl/bench/quickhull | Different algorithm name |
| mis | mpl/bench/mis | Maximal independent set |
| msf | mpl/bench/msf | Minimum spanning forest |
| suffix-array | mpl/bench/suffixarray | Prefix doubling |
| primes | mpl/bench/primes | Sieve of Eratosthenes |
| merge-sort | mpl/bench/mergesort | Parallel merge sort |
| samplesort | mpl/bench/samplesort | Sample-based partitioning |
| tokens | mpl/bench/tokens | Text tokenization |
| nqueens | mpl/bench/nqueens | N-queens backtracking |
| dedup | mpl/bench/dedup | Hash-based deduplication |
| word-count | mpl/bench/wordcount | Map-reduce word count |
| fib | mpl/bench/fib | Parallel Fibonacci |
| shuffle | mpl/bench/shuffle | Fisher-Yates variant |
| grep | mpl/bench/grep | Parallel regex |
| palindrome | - | Custom benchmark |
| parens | - | Custom benchmark |
| mcss | mpl/bench/mcss | Maximum subarray |
| flatten | mpl/bench/flatten | Tree flattening |
| collect | - | Custom benchmark |
| bignum-add | - | Custom benchmark |
| subset-sum | - | Custom benchmark |
| triangle-count | mpl/bench/triangles | Triangle counting |
| connectivity | mpl/bench/connectivity | Union-Find |
| centrality | mpl/bench/centrality | Betweenness centrality |

### 2.3 Parameter Normalization

For fair comparison, normalize problem sizes to achieve similar sequential runtime (~500ms):

```
Benchmark         | Racket N      | MPL N (estimated)
------------------|---------------|------------------
histogram         | 200,000,000   | ~200,000,000
integer-sort      | 50,000,000    | ~50,000,000
bfs               | 8,000,000     | ~8,000,000
nqueens           | 13            | 13
fib               | 42            | 42
subset-sum        | 28            | 28
triangle-count    | 17,000        | ~17,000
connectivity      | 2,000,000     | ~2,000,000
```

## Phase 3: Output Verification

### 3.1 Deterministic Benchmarks

For benchmarks with deterministic output, verify exact match:

| Benchmark | Verification Method |
|-----------|---------------------|
| nqueens | Count of solutions must match known value |
| fib | fib(42) = 267914296 |
| primes | Count of primes up to N |
| subset-sum | Existence of valid subset |
| triangle-count | Exact triangle count |
| connectivity | Component count |

### 3.2 Stochastic Benchmarks

For benchmarks using random input, fix seeds for reproducibility:

```racket
;; Use --seed parameter consistently
(random-seed 42)
```

### 3.3 Order-Independent Verification

Some benchmarks produce results where order doesn't matter:
- **histogram**: Verify bucket counts sum to N
- **dedup**: Verify unique count and containment
- **word-count**: Verify total words and top-K frequencies
- **convex-hull**: Verify hull points are subset of input and convex

### 3.4 Checksum-Based Verification

For large outputs, use checksums:

```racket
(define (checksum-vector v)
  (for/fold ([sum 0]) ([x (in-vector v)])
    (bitwise-xor sum (exact->inexact->fixnum x))))
```

## Phase 4: Cross-Validation Protocol

### 4.1 Generate Reference Outputs

Create reference outputs from sequential implementations:

```bash
# Generate reference outputs
mkdir -p reference-outputs
for bench in histogram integer-sort ...; do
  racket benchmarks/mpl/$bench.rkt --workers 1 --repeat 1 \
    --output-reference reference-outputs/$bench.dat
done
```

### 4.2 Validate Parallel Against Sequential

Every benchmark already does this:

```racket
#:check (λ (iteration result)
          (when (and seq-result (not (equal? seq-result result)))
            (error 'benchmark "parallel result mismatch")))
```

### 4.3 Cross-Language Validation (Optional)

For benchmarks with known correct outputs:

1. Run MPL benchmark with same parameters
2. Export MPL output to file
3. Compare Racket output against MPL output
4. Document any differences (floating-point tolerance, etc.)

## Phase 5: Speedup Analysis

### 5.1 Expected Speedup Categories

Based on algorithm characteristics:

| Category | Expected Speedup | Benchmarks |
|----------|------------------|------------|
| Embarrassingly parallel | Near-linear | nqueens, fib, subset-sum, histogram |
| Good parallel | 3-6x | triangle-count, connectivity, bfs, convex-hull |
| Memory-bound | 1.5-2x | merge-sort, suffix-array, flatten |
| Sequential bottleneck | ~1x | centrality, bignum-add |
| Parallel overhead | <1x | mcss, shuffle |

### 5.2 Speedup Reporting

Generate speedup table:

```
Benchmark       | Seq(ms) | P2    | P4    | P6    | P8    | Max Speedup
----------------|---------|-------|-------|-------|-------|------------
nqueens         | 1635    | 852   | 435   | 302   | 204   | 8.0x
fib             | 771     | 405   | 215   | 160   | 193   | 4.0x
histogram       | 515     | 270   | 145   | 105   | 86    | 6.0x
...
```

### 5.3 Efficiency Analysis

Calculate parallel efficiency:

```
Efficiency = Speedup / Workers

Benchmark       | 2W    | 4W    | 6W    | 8W
----------------|-------|-------|-------|-------
nqueens         | 96%   | 94%   | 90%   | 100%
fib             | 95%   | 90%   | 80%   | 50%
...
```

## Phase 6: Ground Truth Database

### 6.1 Create Reference Database

Store verified outputs for regression testing:

```
ground-truth/
├── histogram-n200M-seed42.checksum
├── nqueens-n13.count
├── fib-n42.result
├── triangle-count-n17000-e1M-seed42.count
└── ...
```

### 6.2 Automated Verification Script

```bash
#!/bin/bash
# verify-ground-truth.sh

for bench in "${benchmarks[@]}"; do
  expected=$(cat ground-truth/$bench.expected)
  actual=$(racket benchmarks/mpl/$bench.rkt --workers 1 --verify-only)
  if [ "$expected" != "$actual" ]; then
    echo "FAIL: $bench expected $expected got $actual"
    exit 1
  fi
done
echo "All ground truth checks passed"
```

## Phase 7: MPL Comparison Study

### 7.1 Metrics to Compare

| Metric | Description |
|--------|-------------|
| Absolute speedup | Racket vs MPL at 8 workers |
| Relative efficiency | How close to linear scaling |
| Sequential overhead | Racket/MPL sequential ratio |
| GC impact | GC time as % of total |

### 7.2 Expected Findings

Based on language characteristics:

1. **MPL advantages**:
   - Native parallel primitives (ForkJoin, par, parfor)
   - Efficient work-stealing scheduler
   - No GC pause overhead during parallel execution
   - Unboxed arrays and records

2. **Racket characteristics**:
   - OS threads via thread pools (not green threads)
   - GC-managed memory (potential pauses)
   - Flexible but less optimized parallelism
   - Rich ecosystem and tooling

### 7.3 Documentation

Create comparison report:

```markdown
## Benchmark: triangle-count

### Parameters
- Vertices: 17,000
- Edges: 1,000,000
- Seed: 42

### Results
| Workers | Racket (ms) | MPL (ms) | Racket Speedup | MPL Speedup |
|---------|-------------|----------|----------------|-------------|
| 1       | 460         | 380      | 1.0x           | 1.0x        |
| 8       | 118         | 52       | 3.9x           | 7.3x        |

### Analysis
Racket achieves 53% of MPL's speedup due to:
- Channel overhead vs. work-stealing
- GC pauses during parallel execution
```

## Execution Timeline

1. **Phase 1**: Run Racket baseline (1-2 hours)
2. **Phase 2**: Set up MPL comparison (if MPL available)
3. **Phase 3**: Verify all outputs match
4. **Phase 4**: Cross-validation
5. **Phase 5**: Generate speedup analysis
6. **Phase 6**: Build ground truth database
7. **Phase 7**: Final comparison report

## Success Criteria

1. All 27 benchmarks produce correct output
2. Sequential and parallel outputs match for all benchmarks
3. At least 15 benchmarks show >2x speedup with 8 workers
4. Ground truth database established for regression testing
5. Comparison with MPL documented (if available)

## Next Steps

1. Run `racket run-mpl-benchmarks.rkt` with all benchmarks
2. Review generated HTML visualization
3. Create ground-truth database from verified outputs
4. Document any discrepancies or issues found
