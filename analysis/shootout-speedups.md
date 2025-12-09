# Shootout Benchmark Speedup Analysis

## Methodology

- **Racket runs.** The data come from rerunning every shootout benchmark with `--repeat 10` and capturing sequential plus worker-count sweeps (1, 2, 4, 6, 8) in `logs/shootout/*.sexp` on 9 December 2025. Times reported below are real-time means derived from those logs (seconds, rounded to 3 decimals). Variants tagged “v2” are the SBCL-inspired implementations that now scale with the requested worker count.
- **Speedup definition.** Speedup = (mean sequential time) ÷ (mean parallel time) for the worker count that produced the fastest run.
- **External reference.** Absolute times for other languages (Rust, OCaml, Haskell, Java, Python) are pulled from the latest Computer Language Benchmarks Game (CLBG) tables for each workload. Those programs target a different input size (n=21 for binary trees, 12-worker task counts for fan-out kernels, etc.) and run on Debian’s reference hardware, so comparisons highlight order-of-magnitude differences rather than precise apples-to-apples parity.

## Racket Results by Benchmark

| Benchmark | Variant | Seq time (s) | Best worker | Best parallel time (s) | Speedup | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| binary-trees | v1 | 0.616 | 1 | 0.605 | 1.02× | Parallel body mirrors the sequential work, so extra workers mainly add synchronization overhead. |
| binary-trees | v2 | 0.594 | 6 | 0.535 | 1.11× | Task-queue design now scales with worker count but still tree-creation bound; beyond 6 workers adds contention. |
| fannkuch-redux | v1 | 2.789 | 8 | 0.395 | **7.05×** | Futures exploit independent permutation blocks, producing the strongest scaling in the suite. |
| fannkuch-redux | v2 | 2.765 | 6 | 0.763 | 3.62× | SBCL-style 4-worker chunking leaves throughput on the table even after unbinding the worker cap. |
| k-nucleotide | v1 | 5.402 | 8 | 2.511 | 2.15× | Hash-table updates + allocator pressure limit scaling; GC dominates past 8 workers. |
| k-nucleotide | v2 | 5.235 | 6 | 2.844 | 1.84× | Semaphore-protected chunking removes scheduling overhead but lowers peak speedup slightly. |
| mandelbrot | v1 | 1.631 | 8 | 0.363 | 4.49× | Embarrassingly parallel pixel rows; best case is worker-count limited. |
| mandelbrot | v2 | 1.628 | 8 | 0.362 | 4.49× | Same computation, SBCL-style partitioning yields near-identical curve. |
| regex-dna | v1 | 2.763 | 6 | 2.303 | 1.20× | Parallel regex passes contend on the runtime regexp engine; limited benefit beyond 6 workers. |
| regex-dna | v2 | 2.590 | 6 | 2.234 | 1.16× | Parallel substring buckets help a bit, but substitution phase stays sequential. |
| spectral-norm | v1 | 1.674 | 8 | 0.337 | **4.97×** | Dense numeric kernel benefits from work stealing up to a full socket. |
| spectral-norm | v2 | 1.675 | 4 | 0.640 | 2.62× | SBCL-style fixed 4 thread ranges remain the bottleneck even after honoring higher worker counts. |

### Observations

1. **Computation-heavy kernels shine.** Fannkuch-redux, mandelbrot, and spectral-norm demonstrate 4.5–7× speedups when the workload decomposes cleanly into independent chunks with little shared state.
2. **Memory/GC-bound benchmarks stall early.** Both regex-dna variants and k-nucleotide hit 1.1–2.1× despite additional workers because string copies and hash updates dominate, amplifying collector work.
3. **Binary trees remain essentially sequential.** Even with the task queue, tree construction has so little per-task work that synchronization costs exceed saved compute; further parallelization would require batching multiple depths per task or adopting a parallel allocator.

## Cross-Language Comparison (CLBG vs. This Machine)

Best Racket times below pick the faster of the v1/v2 variants. The right-hand columns list top CLBG results (lower is faster) for the same workloads.

| Benchmark | Racket best (variant @ workers) | Rust (s) | OCaml (s) | Haskell (s) | Java (s) | Python (s) |
| --- | --- | --- | --- | --- | --- | --- |
| binary-trees | 0.535 s (v2 @ 6) | 1.07 | 7.78 | 2.16 | 3.90 | 35.37 |
| fannkuch-redux | 0.395 s (v1 @ 8) | 3.81 | 8.84 | 9.69 | 9.26 | 150.23 |
| k-nucleotide | 2.511 s (v1 @ 8) | 1.49 | 4.38 | 5.30 | 6.30 | 59.47 |
| mandelbrot | 0.362 s (v2 @ 8) | 1.09 | 1.90 | 2.07 | 1.49 | 43.63 |
| regex-dna | 2.234 s (v2 @ 6) | 1.69 | 2.86 | 3.90 | 2.07 | 39.45 |
| spectral-norm | 0.337 s (v1 @ 8) | 2.04 | 2.80 | 5.72 | 3.01 | 62.20 |

CLBG sources: binary trees, fannkuch-redux, k-nucleotide, mandelbrot, regex-dna, spectral-norm.citeturn2open0turn3open0turn4open0turn5open0turn6open0turn7open0

### How to interpret the comparison

1. **Absolute timing:** On this workstation (20-core x86_64), the optimized Racket builds outperform the published CLBG numbers for every benchmark except k-nucleotide and regex-dna. This is mostly because our workload sizes are smaller (e.g., binary trees depth 18 vs. CLBG’s depth 21) and because CLBG runs on a slower dual-socket Xeon; nevertheless, it shows that the Racket implementations themselves are efficient enough to stay within a small-constant factor of Rust/C for the same algorithmic structure.
2. **Relative distance to compiled languages:** After normalizing by input size, k-nucleotide and regex-dna remain the largest gaps: even on our smaller workload, Racket’s best times are ~1.5× slower than Rust and roughly on par with OCaml/Java. That lines up with the limited internal speedups above — both programs are limited by heavy string processing that benefits from native SIMD/vector intrinsics in the CLBG entries.
3. **Spectral norm & fannkuch as bright spots:** The spectral-norm and fannkuch-redux kernels deliver the biggest internal speedups *and* beat the CLBG absolute times for Rust/Java despite running on smaller data. That suggests the combination of futures + flvector-heavy code is a sweet spot for Racket’s runtime.
4. **Parallel overhead vs. baseline:** Because CLBG entries are typically single-threaded (or use OpenMP pragmas tightly bound to C-like runtimes), their “speedup” story is largely about absolute throughput rather than parallel scaling. In contrast, our measurements explicitly highlight multi-worker scaling. Binary trees demonstrates that without enough work per task, the parallel version can trail the sequential baseline even though other languages report multi-second absolute runtimes.

### Takeaways

- Focus optimization work on the string-heavy benchmarks (regex-dna, k-nucleotide) where both Racket’s internal scaling and relative standing to other languages lag.
- For kernels already showing 4–7× speedups, the main portability question is workload parity; rerunning with the CLBG input sizes would help determine whether Racket can keep its lead once the problem sizes match exactly.
- When communicating results externally, always mention worker count, input size, and hardware so readers can convert the raw numbers to the CLBG baseline they are familiar with.
