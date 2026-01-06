# Racket Parallelism Performance Summary

This benchmark suite of 38 parallel algorithms reveals both the strengths and limitations of Racket's parallelism primitives.

## Key Findings

### Speedup Results at 8 Workers (20-core machine)

| Benchmark | Sequential | Parallel | Speedup | Category |
|-----------|-----------|----------|---------|----------|
| fib (n=42) | 791ms | 117ms | **6.76x** | Compute-bound |
| primes (n=50M) | 213ms | 63ms | **3.38x** | Compute-bound |
| tokens | 679ms | 369ms | **1.84x** | Text processing |
| bfs (n=4M) | 845ms | 494ms | **1.71x** | Graph algorithm |
| merge-sort (n=5M) | 914ms | 564ms | **1.62x** | Sorting |
| binary-trees | 609ms | 673ms | **0.90x** | Allocation-heavy |

### What Works Well

**Compute-bound workloads achieve excellent speedup:**
- Fibonacci with threshold-based sequential cutoff: 6.76x
- Prime sieve: 3.38x
- These benchmarks have minimal allocation in hot paths

**Graph algorithms show moderate speedup:**
- BFS, MIS, MSF, connectivity: 1.5-2x typical
- Parallelism helps despite irregular access patterns

### What Struggles

**Allocation-heavy workloads suffer from GC contention:**

| Benchmark | Real Time | GC Time | GC % |
|-----------|-----------|---------|------|
| binary-trees (8w) | 673ms | 770ms | 114% |
| tokens (8w) | 369ms | 693ms | 188% |
| merge-sort (8w) | 564ms | 535ms | 95% |

When GC time exceeds real time, it indicates parallel GC is running but contention limits gains. The binary-trees benchmark actually runs *slower* with 8 workers than sequentially due to GC pressure.

## Parallel Efficiency Analysis

**CPU utilization** (cpu-ms / real-ms ratio at 8 workers):
- fib: 914/117 = 7.8x (excellent - nearly ideal)
- primes: 351/63 = 5.6x (good)
- merge-sort: 1464/564 = 2.6x (moderate - GC overhead)
- binary-trees: 1638/673 = 2.4x (poor - mostly GC)

## Conclusions

1. **Racket parallelism is effective for compute-bound tasks** with limited allocation. Speedups of 3-7x at 8 cores are achievable.

2. **GC is the primary bottleneck** for allocation-heavy parallel code. Workloads that allocate heavily per-thread see diminishing returns or slowdowns.

3. **Thread pool pattern works well.** All 38 benchmarks use a consistent thread-pool + channel pattern that provides predictable performance.

4. **Threshold-based cutoffs are essential.** Benchmarks that switch to sequential execution below a threshold (like fib with threshold=30) perform much better than those that parallelize down to small granularities.

5. **Moderate parallelism (2-4 workers) often optimal** for GC-heavy workloads. Going to 8+ workers increases GC contention without proportional speedup.

## Recommendations for Racket Parallel Programming

- **Minimize allocation in parallel hot paths** - pre-allocate buffers, use mutation
- **Use sequential cutoffs** - switch to sequential below problem size threshold
- **Profile GC time** - if GC dominates, reduce parallelism or restructure
- **Consider 4 workers** as a practical sweet spot for mixed workloads
- **Pure compute shines** - embarrassingly parallel numeric code scales well

## Test Environment

- **CPU:** 20-core x86_64
- **OS:** Linux
- **Racket:** 9.0.0.10
- **Benchmarks:** 38 (3 Racket + 8 Shootout + 27 MPL)
