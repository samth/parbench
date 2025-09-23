# NAS Parallel Benchmarks Integration

This directory provides helper scripts for running pre-built NAS Parallel Benchmark (NPB) binaries under the shared logging infrastructure. Because the original NAS sources are licensed separately, they are **not** vendored here. To use these helpers:

1. Obtain the NAS Parallel Benchmarks (version 3.x or later) from the official repository.
2. Build the desired kernels (e.g., `ep.A.x`, `mg.B.x`) with your preferred toolchain.
3. Invoke `benchmarks/nas/run.rkt` with the `--binary` flag pointing to the compiled executable and optional `--arg` flags to select class sizes or thread counts.

Example:

```bash
racket benchmarks/nas/run.rkt \
  --binary /path/to/npb/bin/ep.A.x \
  --label ep-A \
  --arg -t \
  --arg 4 \
  --repeat 3 \
  --log logs/nas-ep.sexp
```

The runner will time each invocation, emit `(benchmark …)` S-expression records, and enforce non-zero exit status checks. Additional kernels can be automated by creating thin wrappers (or config presets) that pass the appropriate arguments and environment variables.
