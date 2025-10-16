# Parallel Benchmark Suite Expansion Plan

## Goals
- Build a reproducible, extensible benchmarking harness that unifies existing Racket Boyer–Moore (both legacy and improved) and Richards workloads with a broader catalogue of parallel benchmarks.
- Re-implement NAS Parallel Benchmarks and MPL Parallel ML Benchmarks in Racket, and integrate the Racket shootout benchmarks to provide coverage of compute, memory, and tasking patterns.
- Provide consistent command-line configuration, result capture, and comparative reporting across all workloads.

## Current Status (as of 2025-10-16)

**Completed:**
- ✅ Core infrastructure (Phases 1-2): Shared CLI, logging, harness, tests
- ✅ Shootout integration (Phase 3): 9 benchmarks with parallel variants (spectral-norm, binary-trees, nbody, fannkuch-redux, mandelbrot, chameneos, fasta, regex-dna, k-nucleotide)
- ✅ NAS benchmarks implementation (Phase 4): 3 kernels (EP, IS, CG) with sequential/parallel variants
- ✅ MPL benchmarks re-implementation (Phase 5): 7 benchmarks (histogram, integer-sort, bfs, mis, msf, suffix-array, convex-hull)
- ✅ Unified CLI orchestration (Phase 6): Suite runner with configuration file support
- ✅ Visualization tools (Phase 8): Analysis library, summarizer, PNG plotting, interactive HTML dashboard
- ✅ Documentation updates: All READMEs reflect current implementation status

**Remaining:**
- ⏳ CI integration (Phase 7): Automated smoke tests
- ⏳ Additional NAS benchmarks (MG, FT) as stretch goals
- ⏳ Additional MPL benchmarks (N-body, Delaunay, etc.) as stretch goals

**Summary:** All major phases complete with 19 benchmarks across 4 suites. Project is ready for production use and comparative analysis.

## Existing Artifacts Review
1. `bmbench.rkt` and `bmbench_improved.rkt`: Boyer–Moore majority benchmarks with sequential and parallel variants, CLI-configurable.
2. `richards.rkt`: Sequential and parallel (futures) Richards benchmark with CLI configuration and harness.
3. `tests/`: RackUnit coverage for Boyer–Moore (baseline + improved) and Richards validation.
4. `AGENTS.md`: Contributor guidelines establishing repository structure expectations.

## High-Level Roadmap
1. Establish suite scaffolding
   - Define top-level layout under a new `benchmarks/` directory.
   - Partition benchmarks into `racket` (existing), `shootout`, `nas`, and `mpl` subdirectories.
   - Standardize CLI entry points and result schemas.
2. Adopt a results collection format
   - Implement line-oriented S-expression logging for per-run metrics.
   - Add helper library for formatting and summary statistics.
3. Re-implement external benchmarks in Racket
   - NAS Parallel Benchmarks (re-implement selected kernels in Racket with sequential and parallel variants).
   - MPL Parallel ML Benchmarks (re-implement selected algorithms from the MPL benchmark suite in Racket with sequential and parallel variants).
4. Add automation/testing
   - Extend RackUnit suite for sanity checks on new harnesses.
   - Provide smoke-test targets using reduced problem sizes.
5. Documentation & CI integration
   - Update `AGENTS.md` with benchmarking standards.
   - Introduce CI pipeline steps (or scripts) to run smoke tests.

## Detailed Task Breakdown

### Phase 1 – Repository Restructuring & Common Utilities
- [x] Create `benchmarks/` directory with subfolders for `racket`, `shootout`, `nas`, and `mpl` workloads.
- [x] Implement `benchmarks/common/` modules providing CLI helpers, benchmarking harness, and deterministic RNG hooks.
- [x] Refactor existing Racket benchmarks to use shared utilities and maintain CLI compatibility.
- [x] Update tests to import relocated modules and keep coverage intact.

### Phase 2 – Result Logging & Reporting
- [x] Define `benchmarks/common/logging.rkt` for metadata + S-expression logging.
- [x] Extend existing benchmarks to support a `--log` flag.
- [x] Create `benchmarks/tools/summarize-results.rkt` (aggregation).
- [x] Document logging usage in `BENCHMARKS.md`.

### Phase 3 – Racket Shootout Benchmarks Integration
- [x] Audit the Racket shootout workloads and identify parallelizable candidates.
- [x] Create `benchmarks/shootout/README.md` describing available workloads and differences.
- [x] Vendor or submodule upstream sources as needed (`benchmarks/shootout/src/`).
- [x] Wrap selected benchmarks (spectral norm, binary trees, n-body, fannkuch-redux, mandelbrot, chameneos) with CLI fronts, logging, and smoke configurations.
- [x] Add sanity tests comparing sequential vs. parallel outputs.
- [x] Update documentation to describe shootout benchmarks and parameters.
- [x] Expand coverage with additional shootout programs (e.g., fasta, regex-dna, k-nucleotide) in future iterations.

### Phase 4 – NAS Parallel Benchmarks Implementation
- [x] Select initial target kernels (EP - Embarrassingly Parallel, IS - Integer Sort, CG - Conjugate Gradient, etc.) and document in `benchmarks/nas/README.md`.
- [x] Implement EP (Embarrassingly Parallel) kernel in Racket with sequential and parallel variants.
- [x] Implement IS (Integer Sort) kernel in Racket with sequential and parallel variants.
- [x] Implement CG (Conjugate Gradient) kernel in Racket with sequential and parallel variants.
- [ ] Implement additional kernels (MG, FT) as time permits.
- [x] Add comprehensive tests for each NAS benchmark implementation (EP, IS, CG).
- [x] Document NAS benchmark implementations, problem classes, and validation procedures.

### Phase 5 – MPL Parallel ML Benchmarks Re-implementation
**IMPORTANT:** Re-implement selected MPL benchmarks in Racket (do NOT wrap the MPL compiler). The goal is to have Racket implementations of the algorithms from the MPL benchmark suite to compare Racket's parallel programming capabilities.

- [x] Research MPLLang parallel-ml-bench repository and identify suitable benchmarks for re-implementation in Racket.
- [x] Create `benchmarks/mpl/README.md` describing which benchmarks are implemented and their algorithmic approach.
- [x] Re-implement selected MPL benchmarks as Racket programs with sequential and parallel variants.
  - [x] Histogram: parallel counting with partitioned reduction
  - [x] Integer Sort: parallel counting sort for bounded ranges
  - [x] BFS: level-synchronous parallel breadth-first search
  - [x] MIS: Maximal Independent Set using Luby's randomized parallel algorithm
  - [x] MSF: Minimum Spanning Forest using Borůvka's algorithm
  - [x] Suffix Array: parallel prefix-doubling algorithm
  - [x] Convex Hull: parallel QuickHull algorithm
- [x] Add verification procedures to ensure algorithmic correctness.
- [x] Add example configurations for quick/standard benchmark runs.
- [x] Integrate MPL benchmarks into run-suite.rkt.

### Phase 6 – Unified CLI & Orchestration
- [x] Implement top-level `benchmarks/run-suite.rkt` enabling `--suite racket|shootout|nas|mpl|all` selection.
- [x] Add configuration file support (`benchmarks/config/*.sexp`) for canonical runs.
- [x] Provide example configs for "quick", "standard", and "stress" suites.

### Phase 7 – CI Integration & Automation (Planned)
- [ ] Add GitHub Actions workflow for smoke tests
- [ ] Implement pre-commit hooks for benchmark validation
- [ ] Create automated regression detection
- [ ] Add performance tracking over time

### Phase 8 – Visualization & Reporting
- [x] Create reusable analysis helpers for log ingestion (`benchmarks/tools/analysis.rkt`).
- [x] Build `benchmarks/tools/plot-results.rkt` using the `plot` library to render benchmark summaries.
- [x] Build `benchmarks/tools/visualize.rkt` for interactive HTML dashboards.
- [x] Document plotting workflow and sample commands.
- [x] Provide examples for all visualization tools.

## Considerations & Open Questions
- NAS Implementation Fidelity: Ensure Racket implementations of NAS kernels maintain algorithmic equivalence with reference implementations while leveraging Racket's parallelism primitives (futures, places).
- MPL Implementation Fidelity: Ensure Racket re-implementations of MPL benchmarks maintain algorithmic equivalence with the MPL reference implementations while leveraging Racket's parallelism primitives.
- Validation: Implement verification procedures to validate correctness of NAS and MPL implementations against known outputs and checksums where available.
- Licensing: Review NAS and MPL specifications to ensure compliance with redistribution and documentation.
- Hardware Metrics: Consider optional integration with system profilers (perf, Linux perf events) for advanced metrics in future iterations.

## Success Criteria
- Running `racket benchmarks/run-suite.rkt --suite all --config benchmarks/config/quick.sexp` executes Racket, shootout (reduced), NAS (reduced), and MPL (smoke) benchmarks, logging results as S-expressions and summarizing statistics.
- CI smoke tests validate harness integrity without requiring heavy dependencies when unavailable (skip with warning).
- Documentation enables contributors to add new benchmarks following established patterns within a single development sprint.
