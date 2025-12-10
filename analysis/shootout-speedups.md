# Shootout Benchmark Parallel Speedups (Racket vs. CLBG Languages)

## 1. Racket Benchmarks: Measured Speedups

All numbers below are derived from the refreshed `logs/shootout/*.sexp` data produced on 9 December 2025 (20-core x86_64 system, `--repeat 10` for every sequential run and each worker count 1/2/4/6/8). Times are real-time means in seconds.

| Benchmark | Variant | Sequential (s) | Best worker | Best parallel (s) | Speedup |
| --- | --- | --- | --- | --- | --- |
| binary-trees | v1 | 0.616 | 1 | 0.605 | 1.02× |
| binary-trees | v2 | 0.594 | 6 | 0.535 | 1.11× |
| fannkuch-redux | v1 | 2.789 | 8 | 0.395 | **7.05×** |
| fannkuch-redux | v2 | 2.765 | 6 | 0.763 | 3.62× |
| k-nucleotide | v1 | 5.402 | 8 | 2.511 | 2.15× |
| k-nucleotide | v2 | 5.235 | 6 | 2.844 | 1.84× |
| mandelbrot | v1 | 1.631 | 8 | 0.363 | 4.49× |
| mandelbrot | v2 | 1.628 | 8 | 0.362 | 4.49× |
| regex-dna | v1 | 2.763 | 6 | 2.303 | 1.20× |
| regex-dna | v2 | 2.590 | 6 | 2.234 | 1.16× |
| spectral-norm | v1 | 1.674 | 8 | 0.337 | **4.97×** |
| spectral-norm | v2 | 1.675 | 4 | 0.640 | 2.62× |

Interpretation:
- The computation-heavy kernels (fannkuch-redux, spectral-norm, mandelbrot) achieve 4.5–7× speedups on 8 workers, showing good scaling of the futures/thread pools.
- Memory/GC-heavy kernels (k-nucleotide, regex-dna) top out around 2× because allocator contention and string handling dominate.
- Both binary-trees variants remain essentially sequential despite the new worker-scaling task queue, indicating that tree construction work-items are too small relative to synchronization overhead.

## 2. What the CLBG Publishes (Rust/OCaml/Haskell/Java/Python)

The Computer Language Benchmarks Game (CLBG) runs each submitted program once per benchmark/input and reports a single elapsed time plus supporting metrics (code size, memory, optionally CPU time summed across threads) on a fixed Debian server (Intel Q6600 @ 2.4 GHz, 4 GB).citeturn0search7 The per-program tables explicitly warn that “some programs are single-threaded—some sequentially use multiple cores,” but no worker-count sweep or sequential baseline is provided.citeturn1view0 Consequently, CLBG publishes absolute runtimes but not *parallel speedups* for Rust, OCaml, Haskell, Java, or Python; at best we can note whether a submission is annotated as multi-threaded (via the “CPU secs” column) but we cannot reconstruct scaling curves analogous to the Racket data above.

### Fastest CLBG absolute times (seconds)

| Benchmark | Rust | OCaml | Haskell | Java | Python |
| --- | --- | --- | --- | --- | --- |
| binary-trees | 1.07citeturn1view0 | 7.78citeturn1view0 | 2.16citeturn1view0 | 3.90citeturn1view0 | 33.37citeturn1view0 |
| fannkuch-redux | 3.81citeturn2view0 | 8.84citeturn2view0 | 9.69citeturn2view0 | 9.26citeturn2view0 | 1.41citeturn2view0 |
| k-nucleotide | 2.57citeturn3view0 | 16.17citeturn3view0 | 23.30citeturn3view0 | 6.25citeturn3view0 | 46.55citeturn3view0 |
| mandelbrot | 0.95citeturn4view0 | 7.60citeturn4view0 | 1.39citeturn4view0 | 3.96citeturn4view0 | 143.13citeturn4view0 |
| regex-dna | 0.78citeturn5view0 | 2.20citeturn5view0 | 1.10citeturn5view0 | 1.38citeturn5view0 | 1.41citeturn5view0 |
| spectral-norm | 0.72citeturn6view0 | 5.34citeturn6view0 | 1.49citeturn6view0 | 1.47citeturn6view0 | 90.37citeturn6view0 |

> **Important caveat:** CLBG uses different input sizes (e.g., binary trees depth 21) and a slower reference CPU, so these absolute times are *not* directly comparable to the Racket numbers gathered on a modern workstation. They simply illustrate the order of magnitude for each language’s best submission.

## 3. Comparing Speedups

Because CLBG publishes only a single runtime per submission—with no paired sequential vs. parallel runs—there is no way to compute speedups for Rust, OCaml, Haskell, Java, or Python analogous to the Racket data above. The site even warns readers to “compare secs and CPU secs” to guess whether a program used multiple cores, but that still yields at most a binary “parallel or not” flag, not a scaling curve.citeturn1view0 Therefore:

- **Racket:** We have explicit sequential baselines and multi-worker sweeps, so we can quantify 4–7× gains on the most parallel-friendly workloads.
- **Other languages on CLBG:** Only absolute runtimes are published; the implied speedup is whatever the submitter achieved relative to an unpublished single-threaded baseline. In practice, many fastest entries are single-threaded (CPU secs ≈ wall secs), which means their published “speedup” is effectively 1× even if the language could scale.

## 4. Takeaways

1. **Racket’s parallel scaling is directly measurable.** The instrumentation built into this repo makes it easy to report sequential vs. multi-worker behavior, and the results show genuinely strong scaling on compute-bound kernels.
2. **External comparisons should stress methodology.** When presenting numbers next to CLBG languages, note that their figures are single data points on different hardware and inputs; they do *not* demonstrate parallel speedups. Any claims about relative scaling would require running those languages through the same multi-worker harness used here.
3. **Future work.** To produce a like-for-like comparison, rerun the Rust/OCaml/Haskell/Java/Python reference implementations locally using this suite’s log+analysis pipeline. That would yield both absolute times on identical hardware and true speedup metrics instead of single CLBG runtimes.
