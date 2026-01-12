# Repository Guidelines

## Project Structure & Module Organization
The codebase is intentionally compact: `bmbench.rkt` at the repository root contains the Boyer–Moore majority benchmark plus helper routines for vector generation and parallel merging. A `module+ main` block drives the benchmark run; keep reusable functions above it so they remain importable. If you introduce support code (e.g., profiling tools or reusable fixtures), place them in new modules under `src/` and add a matching `tests/` subtree for clarity.

## Build, Test, and Development Commands
- `racket bmbench.rkt` — run the benchmark with the default workload and print timing for sequential and parallel variants.
- `racket -l- raco fmt bmbench.rkt` — format the main module in place; run this before committing layout changes.
- `raco test tests` — execute RackUnit tests once a `tests/` directory exists; add `-x` to stop on the first failure during debugging.

## Coding Style & Naming Conventions
Use standard Racket indentation (2 spaces) and prefer descriptive, hyphenated identifiers (`vector-boyer-moore-majority/parallel`). Keep top-level definitions grouped by responsibility (scanning, merging, generators, benchmarks) and document tunable parameters such as `N` with concise comments. Avoid trailing whitespace and keep lines under ~100 characters to preserve readability in diffs.

## Testing Guidelines
Adopt RackUnit for unit and property checks. Mirror production modules with `tests/<module>-test.rkt` files that `require rackunit` and the module under test. Ensure parallel behavior is covered by comparing sequential and parallel results on randomized vectors. When benchmarks change, capture representative output in the PR description rather than hard-coding timings into tests.

## Commit & Pull Request Guidelines
There is no published Git history yet, so establish a clean precedent: use conventional, imperative commit subjects (e.g., `Add rackunit harness`). Each PR should explain the motivation, summarize benchmark deltas (machine, Racket version, `N` value), and link related issues. Include any required reproduction steps or configuration tweaks so results remain reproducible across agents.

## Performance & Benchmark Notes
`N` at the top of `bmbench.rkt` governs workload size; document any adjustments and restore the default before merging unless a new default is justified. Run benchmarks on an otherwise idle system, mention processor counts used, and close any temporary thread pools you create to keep comparisons fair. Favor deterministic inputs by seeding RNGs when investigating regressions.
