# Racket Shootout Upstream Snapshots

This directory vendors the reference implementations from the Computer
Language Benchmarks Game that inspired the harnessed versions under
`benchmarks/shootout/`.

Each file captures the original sequential program (lightly normalized
for consistent indentation) so that contributors can compare behaviour,
verify algorithmic intent, and ensure our extended variants remain
faithful to the baseline sources.

All files are provided as plain Racket scripts without dependencies on
the shared benchmarking utilities. They are kept for historical context
and are not invoked by the suite—they are deliberately *not* required
when running `racket benchmarks/run-suite.rkt`.

Source: <https://benchmarksgame-team.pages.debian.net/benchmarksgame/>
(retrieved 2025-10-02).

License: The Benchmark Game places submitted programs in the public
domain. See the upstream site for details.
