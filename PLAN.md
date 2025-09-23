# Parallel Benchmark Suite Expansion Plan

## Goals
- Build a reproducible, extensible benchmarking harness that unifies existing Racket Boyer–Moore (both legacy and improved) and Richards workloads with a broader catalogue of parallel benchmarks.
- Integrate respected external suites (NAS Parallel Benchmarks, MPL’s Parallel ML Benchmarks, and the Racket shootout benchmarks) to provide coverage of compute, memory, and tasking patterns.
- Provide consistent command-line configuration, result capture, and comparative reporting across all workloads.

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
3. Import external benchmarks
   - NAS Parallel Benchmarks (focus on C/Fortran kernels wrapped for CLI usage).
   - MPLLang Parallel ML Benchmarks (target selected workloads with minimal dependencies).
4. Add automation/testing
   - Extend RackUnit suite for sanity checks on new harnesses.
   - Provide smoke-test targets using reduced problem sizes.
5. Documentation & CI integration
   - Update `AGENTS.md` with benchmarking standards.
   - Introduce CI pipeline steps (or scripts) to run smoke tests.

## Detailed Task Breakdown

### Phase 1 – Repository Restructuring & Common Utilities
- [ ] Create `benchmarks/` directory with subfolders:
  - `benchmarks/racket/` (move or symlink current Racket benchmarks for clarity).
  - `benchmarks/shootout/`, `benchmarks/nas/`, and `benchmarks/mpl/` for external imports.
- [ ] Implement `benchmarks/common/` module providing:
  - CLI parsing helpers (shared option definitions, numeric parsing).
  - `run-benchmark` helper that handles GC, timing, and structured result emission ((benchmark ...) S-expressions).
  - Seeded RNG utility for deterministic runs.
- [ ] Refactor `bmbench.rkt`, `bmbench_improved.rkt`, and `richards.rkt` to use shared utilities without breaking current CLI flags (ensure backward compatibility via thin wrappers or updated command-line definitions).
- [ ] Update tests to import relocated modules (adjust `require` paths) and ensure they still pass.

### Phase 2 – Result Logging & Reporting
- [ ] Define `benchmarks/common/logging.rkt` with functions to:
  - Emit benchmark metadata: benchmark name, implementation tag, CLI parameters, hardware info stub.
  - Write timing results to stdout and an optional `.sexp` log file (one S-expression per run).
- [ ] Extend existing benchmarks to optionally log to S-expression files (new `--log` flag defaulting to stdout-only).
- [ ] Create `benchmarks/tools/summarize-results.rkt` for aggregating S-expression logs (mean, stddev, speedups, etc.).
- [ ] Document usage in `BENCHMARKS.md` (new file) covering logging convention and summary scripts.

### Phase 3 – Racket Shootout Benchmarks Integration
- [x] Audit the existing Racket shootout benchmark implementations (e.g., under Racket’s `benchmarks/shootout` directory) and identify parallelizable workloads.
- [x] Create `benchmarks/shootout/README.md` summarizing included benchmarks, inputs, and any deviations from upstream.
- [ ] Vendor or submodule the shootout sources under `benchmarks/shootout/src/`, keeping licensing notices intact.
- [x] Wrap each selected benchmark with a Racket CLI front-end that:
  - [x] Accepts problem size / iteration parameters consistent with upstream usage.
  - [x] Emits timing results via the shared S-expression logging helper.
  - [x] Provides reduced-size configurations suitable for CI smoke tests.
- [x] Add RackUnit or property-based sanity checks where feasible (e.g., comparing outputs against known reference results for small inputs).
- [x] Update documentation to describe the shootout subset and any build prerequisites.

### Phase 4 – NAS Parallel Benchmarks Integration
- [x] Decide target kernels (e.g., EP, MG, FT) balancing implementation effort and coverage. *(Initial tooling targets EP-style executables; additional kernels can reuse the runner.)*
- [x] Add `benchmarks/nas/README.md` describing benchmark variants and build requirements.
- [ ] For each chosen NAS kernel:
  - [ ] Vendor or fetch source (consider git submodule) under `benchmarks/nas/src/`.
  - [x] Create Racket wrapper to execute a compiled kernel with configurable arguments.
  - [x] Normalize output to S-expressions via `benchmarks/common/logging.rkt`.
  - [ ] Provide reduced problem size (e.g., class `S`) for CI smoke tests.
- [x] Extend `PLAN.md` once integration strategy is validated (note dependencies, compilers needed).

### Phase 5 – MPL Parallel ML Benchmarks Adoption
- [ ] Mirror repository (git submodule pointing to `github.com/MPLLang/parallel-ml-bench`).
- [ ] Identify required runtime (MLton, Poly/ML, etc.) and document prerequisites.
- [ ] Write wrapper scripts to:
  - [ ] Build selected benchmarks with tuning for local environment.
  - [ ] Execute with configurable input sizes.
  - [ ] Capture timing data and emit S-expressions via logging helper.
- [ ] Provide translation layer if outputs are not easily parseable (e.g., parse textual results).
- [ ] Add smoke-test configuration using smallest dataset.

### Phase 6 – Unified CLI & Orchestration
- [ ] Implement top-level `benchmarks/run-suite.rkt` enabling:
  - Selection of benchmarks via `--suite racket|shootout|nas|mpl|all`.
  - Parallel execution scheduling (control concurrency to avoid interference).
  - Aggregated reporting (speedups, comparisons between baseline and improved versions).
- [ ] Add configuration file support (`benchmarks/config/*.sexp`) to define canonical runs.
- [ ] Provide example configs for “quick”, “standard”, and “stress” suites.

### Phase 7 – Testing, CI, and Documentation
- [ ] Extend RackUnit to cover logging helpers and CLI parsing edge cases (invalid parameters, missing dependencies).
- [ ] Add lint/test job to CI pipeline executing `raco test tests` and smoke benchmarks with reduced sizes (guarded to skip if dependencies absent).
- [ ] Update documentation:
  - `AGENTS.md`: mention `benchmarks/` layout, logging expectations, external dependency management.
  - New `BENCHMARKS.md` providing setup instructions for Racket shootout, NAS, and MPL suites, environment variables, and troubleshooting.
- [ ] Provide user-facing examples in README (or dedicated doc) illustrating combined benchmark runs and sample S-expression output.

## Considerations & Open Questions
- Dependency Management: Determine packaging approach (submodules vs. tarball snapshots) to keep repository manageable.
- Toolchain Availability: Document required compilers (e.g., Fortran for NAS) and ML runtimes, with detection scripts that gracefully skip unavailable benchmarks.
- Licensing: Review NAS and MPL licenses to ensure compliance with redistribution and documentation.
- Hardware Metrics: Consider optional integration with system profilers (perf, Linux perf events) for advanced metrics in future iterations.

## Success Criteria
- Running `racket benchmarks/run-suite.rkt --suite all --config benchmarks/config/quick.sexp` executes Racket, shootout (reduced), NAS (reduced), and MPL (smoke) benchmarks, logging results as S-expressions and summarizing statistics.
- CI smoke tests validate harness integrity without requiring heavy dependencies when unavailable (skip with warning).
- Documentation enables contributors to add new benchmarks following established patterns within a single development sprint.
