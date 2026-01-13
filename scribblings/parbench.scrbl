#lang scribble/manual

@require[scribble/bnf]

@title[#:tag "parbench"]{@exec{raco parbench}: Run parallel benchmarks}
@author{Sam Tobin-Hochstadt}

A comprehensive benchmarking suite for evaluating parallel performance in Racket.

The @exec{raco parbench} command runs parallel benchmarks and reports timing
results. It includes 36 benchmarks across three suites:

@itemlist[
 @item{@bold{MPL} (27 benchmarks) --- Graph algorithms, sorting, numeric computations, and text processing}
 @item{@bold{Shootout} (6 benchmarks) --- Classic language benchmark game workloads}
 @item{@bold{Racket} (3 benchmarks) --- Native Racket benchmarks}
]

The package provides both sequential and parallel implementations of each benchmark,
allowing you to measure parallel speedup and scalability across different core counts.

@section{Installation}

Install the package using @exec{raco pkg}:

@codeblock{
raco pkg install parbench
}

Or link from a local clone:

@verbatim{
git clone https://github.com/example/parbench
cd parbench
raco pkg install --link .
}

@subsection{Requirements}

@itemlist[
 @item{Racket 9.0 or later}
 @item{4+ CPU cores recommended}
 @item{Linux or macOS (Windows via WSL)}
]

@section{Quick Start}

After installation, use @exec{raco parbench} to run benchmarks:

@verbatim{
raco parbench fib              # Run single benchmark
raco parbench mpl              # Run MPL suite (27 benchmarks)
raco parbench shootout         # Run Shootout suite (6 benchmarks)
raco parbench racket           # Run Racket suite (3 benchmarks)
raco parbench --quick          # Quick smoke test
raco parbench -v fib           # Verbose output
raco parbench --save fib       # Save log files
raco parbench --html fib       # Save logs + HTML report
}

By default, @exec{raco parbench} runs quietly and prints a summary table without
saving files. Use @DFlag{save} to save log files or @DFlag{html} to also generate
HTML reports.

Alternatively, you can run the @exec{./bench} script directly from the repository
root with identical arguments.

@subsection{Example Output}

@verbatim{
$ raco parbench --quick fib
Parbench (quick mode)

Running mpl benchmarks...
  fib

========================================
  Results Summary
========================================

                              seq                  1 workers               4 workers
Benchmark               mean/median/min         mean/median/min         mean/median/min
--------------------------------------------------------------------------------------------
fib                     785.0/788.0/764         798.0/797.0/795         216.0/217.0/209
}

@section{Command-Line Reference}

The @exec{raco parbench} command accepts the following options:

@subsection{Mode Options}

@itemlist[
 @item{@DFlag{quick} or @Flag{q} --- Quick smoke test mode. Runs 3 iterations with
       limited core counts (1 and 4).}
 @item{@DFlag{verbose} or @Flag{v} --- Show detailed per-benchmark output during execution.}
]

@subsection{Output Options}

@itemlist[
 @item{@DFlag{save} or @Flag{s} --- Save results to log files in the output directory.
       Creates a timestamped subdirectory under @filepath{results/}.}
 @item{@DFlag{html} --- Generate an HTML visualization report. Implies @DFlag{save}.}
 @item{@DFlag{output} @nonterm{dir} or @Flag{o} @nonterm{dir} --- Set the output base
       directory. Default: @filepath{./results}}
 @item{@DFlag{update} @nonterm{dir} or @Flag{u} @nonterm{dir} --- Add results to an
       existing run directory. Implies @DFlag{save}.}
]

@subsection{Benchmark Configuration}

@itemlist[
 @item{@DFlag{iterations} @nonterm{n} or @Flag{i} @nonterm{n} --- Number of timed
       iterations per benchmark. Default: 10.}
 @item{@DFlag{work} @nonterm{factor} or @Flag{w} @nonterm{factor} --- Scale problem sizes
       by the given factor. Use @racket[0.1] for 10% of normal size, @racket[0.001] for
       very quick smoke tests.}
 @item{@DFlag{cores} @nonterm{counts} or @Flag{c} @nonterm{counts} --- Specify worker
       counts. Accepts comma-separated values (@racket{1,4,8}) or ranges (@racket{1-8}).}
]

@subsection{Information Options}

@itemlist[
 @item{@DFlag{list} or @Flag{l} --- List all available benchmarks and exit.}
 @item{@DFlag{dry-run} or @Flag{n} --- Show commands that would be executed without
       actually running them.}
 @item{@DFlag{help} or @Flag{h} --- Show help message and exit.}
]

@subsection{Specifying Benchmarks}

You can specify what to run as positional arguments:

@itemlist[
 @item{@bold{Suite names}: @exec{mpl}, @exec{shootout}, @exec{racket}, or @exec{all}}
 @item{@bold{Individual benchmarks}: @exec{fib}, @exec{histogram}, @exec{binary-trees}, etc.}
 @item{@bold{Multiple benchmarks}: @exec{raco parbench fib histogram bfs}}
 @item{@bold{Default (no arguments)}: Runs all benchmarks}
]

@section{Benchmark Suites}

@subsection{MPL Benchmarks (27)}

The MPL benchmarks are parallel implementations of algorithms from the MPL benchmark suite.

@subsubsection{Graph Algorithms}

@itemlist[
 @item{@bold{bfs} --- Breadth-first search}
 @item{@bold{mis} --- Maximal independent set}
 @item{@bold{msf} --- Minimum spanning forest}
 @item{@bold{connectivity} --- Graph connectivity}
 @item{@bold{triangle-count} --- Triangle counting}
 @item{@bold{centrality} --- Betweenness centrality}
 @item{@bold{convex-hull} --- Convex hull computation}
]

@subsubsection{Sorting}

@itemlist[
 @item{@bold{integer-sort} --- Parallel integer sorting}
 @item{@bold{merge-sort} --- Parallel merge sort}
 @item{@bold{samplesort} --- Sample sort algorithm}
 @item{@bold{suffix-array} --- Suffix array construction}
]

@subsubsection{Numeric}

@itemlist[
 @item{@bold{histogram} --- Parallel histogram computation}
 @item{@bold{primes} --- Prime number sieve}
 @item{@bold{fib} --- Parallel Fibonacci}
 @item{@bold{nqueens} --- N-Queens solver}
 @item{@bold{mcss} --- Maximum contiguous subsequence sum}
 @item{@bold{subset-sum} --- Subset sum problem}
 @item{@bold{bignum-add} --- Big number addition}
]

@subsubsection{Text Processing}

@itemlist[
 @item{@bold{tokens} --- Tokenization}
 @item{@bold{word-count} --- Word frequency counting}
 @item{@bold{grep} --- Parallel pattern matching}
 @item{@bold{dedup} --- Deduplication}
 @item{@bold{palindrome} --- Palindrome detection}
 @item{@bold{parens} --- Parentheses matching}
]

@subsubsection{Other}

@itemlist[
 @item{@bold{flatten} --- Parallel list flattening}
 @item{@bold{collect} --- Parallel collection}
 @item{@bold{shuffle} --- Parallel shuffle}
]

@subsection{Shootout Benchmarks (6)}

Classic benchmarks from the Computer Language Benchmarks Game:

@itemlist[
 @item{@bold{binary-trees} --- Binary tree allocation and traversal}
 @item{@bold{spectral-norm} --- Eigenvalue approximation}
 @item{@bold{fannkuch-redux} --- Pancake flipping permutations}
 @item{@bold{mandelbrot} --- Mandelbrot set generation}
 @item{@bold{k-nucleotide} --- Nucleotide frequency counting}
 @item{@bold{regex-dna} --- DNA pattern matching}
]

@subsection{Racket Benchmarks (3)}

Native Racket benchmarks:

@itemlist[
 @item{@bold{bmbench} --- Boyer-Moore majority voting algorithm}
 @item{@bold{richards} --- Richards device scheduler simulation}
 @item{@bold{rows1b} --- Synthetic row processing workload}
]

@section{Running Individual Benchmarks}

Each benchmark can be run directly using @exec{racket}:

@verbatim{
racket benchmarks/mpl/fib.rkt --n 42 --threshold 30 --workers 4 --repeat 5
racket benchmarks/mpl/histogram.rkt --n 200000000 --workers 8 --log results/hist.sexp
racket benchmarks/mpl/bfs.rkt --n 8000000 --graph-type grid --workers 4
racket benchmarks/shootout/binary-trees.rkt --n 18 --workers 8 --repeat 10
racket benchmarks/shootout/mandelbrot.rkt --n 4000 --workers 8
racket benchmarks/racket/bmbench.rkt --n 1000000 --workers 4 --repeat 10
racket benchmarks/racket/richards.rkt --iterations 100 --workers 8
}

@subsection{Common Benchmark Options}

All individual benchmarks support these options:

@itemlist[
 @item{@DFlag{workers} @nonterm{n} --- Number of parallel workers}
 @item{@DFlag{repeat} @nonterm{n} --- Number of timed iterations}
 @item{@DFlag{log} @nonterm{file} --- Write S-expression results to file}
 @item{@DFlag{skip-sequential} --- Skip the sequential baseline run}
]

Each benchmark may have additional benchmark-specific options (e.g., @DFlag{n} for
problem size, @DFlag{threshold} for parallelism cutoff).

@section{Log Format}

Benchmark results are recorded as S-expressions with the following structure:

@codeblock|{
(benchmark
  (name histogram)
  (variant parallel)
  (iteration 1)
  (repeat 10)
  (metrics (cpu-ms 520) (real-ms 515) (gc-ms 12))
  (params (n 200000000) (workers 8))
  (metadata (timestamp 1758661801) (racket-version "8.18"))
  (status ok))
}|

@itemlist[
 @item{@bold{name} --- Benchmark identifier}
 @item{@bold{variant} --- Either @racket['sequential] or @racket['parallel]}
 @item{@bold{iteration} --- Current iteration number (1-based)}
 @item{@bold{repeat} --- Total number of iterations}
 @item{@bold{metrics} --- Timing data:
   @itemlist[
     @item{@bold{cpu-ms} --- CPU time in milliseconds}
     @item{@bold{real-ms} --- Wall-clock time in milliseconds}
     @item{@bold{gc-ms} --- Garbage collection time in milliseconds}
   ]}
 @item{@bold{params} --- Benchmark parameters (problem size, worker count, etc.)}
 @item{@bold{metadata} --- Run metadata (timestamp, Racket version)}
 @item{@bold{status} --- Either @racket['ok] or @racket['error]}
]

@section{Analysis Tools}

The package includes tools for analyzing and visualizing benchmark results.

@subsection{Summarizing Results}

Compute statistics (mean, standard deviation, min, max) from log files:

@verbatim{
racket benchmarks/tools/summarize-results.rkt results/*.sexp
}

@subsection{Generating Plots}

Create PNG plots from benchmark results:

@verbatim{
racket benchmarks/tools/plot-results.rkt \
  --input results/*.sexp \
  --metric real \
  --output plots/benchmark.png
}

@subsection{HTML Dashboard}

Generate an interactive HTML visualization dashboard:

@verbatim{
racket benchmarks/tools/visualize.rkt \
  --log-dir results \
  --output dashboard.html
}

@section{Configuration Files}

Pre-defined configurations are available in @filepath{benchmarks/config/}:

@tabular[#:style 'boxed
         #:column-properties '(left left)
         (list (list @bold{File} @bold{Purpose})
               (list @filepath{quick.sexp} "Fast smoke tests (small sizes, 1 repeat)")
               (list @filepath{standard.sexp} "Typical benchmarking (moderate sizes)")
               (list @filepath{stress.sexp} "Large problems (comprehensive evaluation)"))]

Use with the suite runner:

@verbatim{
racket benchmarks/run-suite.rkt --suite all --config benchmarks/config/quick.sexp
}

@section{Repository Structure}

@verbatim{
parbench/
+-- bench                    # Unified benchmark runner script
+-- raco-parbench.rkt        # Raco command wrapper
+-- info.rkt                 # Package metadata
+-- README.md                # Project overview
+-- BENCHMARKS.md            # CLI reference
+-- benchmarks/
|   +-- common/              # Shared CLI, logging infrastructure
|   +-- mpl/                 # MPL parallel algorithms (27)
|   +-- shootout/            # Shootout benchmarks (6)
|   +-- racket/              # Racket benchmarks (3)
|   +-- tools/               # Analysis and visualization
|   +-- config/              # Configuration files
+-- tests/                   # RackUnit test suite
+-- scribblings/             # This documentation
}

@section{License}

Apache 2.0 or MIT, at your option.
