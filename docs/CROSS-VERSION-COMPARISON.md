# Cross-Version Benchmark Comparison and Visualization

Design document for measuring and visualizing benchmark performance changes across different Racket versions.

## Goals

1. **Track improvement** - See how changes have improved performance over time
2. **Measure effectiveness** - Analyze whether a particular change achieved its intended impact
3. **Understand patterns** - Which benchmarks benefited most from specific changes

**Target use case:** Racket core development, comparing 3-5 versions with interactive HTML output.

## Current Infrastructure

- Benchmarks already capture `racket-version` in S-expression metadata
- Results stored in timestamped directories (`results/YYYYMMDD-HHMMSS/`)
- Analysis tools: `analysis.rkt` computes statistics (mean, stddev, min/max)
- Existing HTML dashboard: Chart.js with box plots, line charts, sortable tables
- **Gap:** No cross-version comparison or change impact analysis

---

## Recommended Approach: A/B Comparison Dashboard

A visualization optimized for answering "did this change help?"

### Part 1: Data Collection - Multi-Version Runner

**New file:** `benchmarks/tools/multi-version-runner.rkt`

```bash
# Usage examples
./bench-versions --racket /opt/racket-8.10/bin/racket,/opt/racket-8.11/bin/racket --suite mpl
./bench-versions --racket-dir /opt/racket-versions --suite all
```

**Capabilities:**
- Accept list of Racket executable paths
- Auto-detect version via `racket --version`
- Run benchmarks via subprocess (avoids version conflicts)
- Store results in version-tagged directories

**Directory structure:**
```
results/versions/20260113-150000/
  manifest.sexp              # Versions, params, status
  v8.10/mpl/fib.sexp
  v8.11/mpl/fib.sexp
  v9.0/mpl/fib.sexp
  comparison.html            # Generated dashboard
```

### Part 2: Analysis Enhancements

**Extend:** `benchmarks/tools/analysis.rkt`

New functions:
- `load-summaries-by-version` - Load from multiple version directories
- `compute-version-speedup` - Relative performance between versions
- `statistical-significance` - Welch's t-test for meaningful changes

**Change detection:**
- Flag statistically significant changes (p < 0.05)
- Cohen's d for effect size (small/medium/large improvement)
- Highlight meaningful improvements (> 5% speedup)

### Part 3: Visualization Components

#### View 1: Comparison Overview (Primary)

Side-by-side before/after view with version selectors.

```
+---------------------------------------------------------------------+
|  Compare:  [v8.11 v]  ->  [v9.0 v]                                  |
+---------------------------------------------------------------------+
|  IMPROVED: 24 benchmarks   |  UNCHANGED: 8   |  SLOWER: 4           |
+---------------------------------------------------------------------+
|                                                                     |
|  <-- FASTER (%)  0%  SLOWER (%) -->                                 |
|  histogram  ################                              -15%      |
|  bfs        ##########                                    -10%      |
|  fib        ####                                          -5%       |
|  spectral                                                  0%       |
|  b-trees                                          ####    +5%       |
|                                                                     |
+---------------------------------------------------------------------+
```

**Key features:**
- **Version dropdowns:** Select any two versions to compare (before/after)
- **Summary bar:** Quick count of improved/unchanged/slower benchmarks
- **Butterfly chart:** Horizontal bars showing % change, sorted by improvement
- **One-click answer:** Instantly see if a change was beneficial overall

#### View 2: Benchmark Comparison Detail

Click any benchmark row for side-by-side deep dive:

```
+---------------------------------------------------------------------+
|  histogram: v8.11 -> v9.0                             [-15% faster] |
+---------------------------------------------------------------------+
|                                                                     |
|  Before (v8.11)          After (v9.0)                               |
|  +------------------+    +------------------+                       |
|  | mean: 1234 ms    |    | mean: 1049 ms    |   delta -185 ms      |
|  | min:  1180 ms    |    | min:  1020 ms    |                       |
|  | max:  1310 ms    |    | max:  1095 ms    |                       |
|  | stddev: 42 ms    |    | stddev: 25 ms    |   (variance down)    |
|  +------------------+    +------------------+                       |
|                                                                     |
|  Distribution (box plots side-by-side)                              |
|  v8.11: --[====|====]--                                             |
|  v9.0:     -[==|==]-                                                |
|                                                                     |
|  Worker scaling comparison:                                         |
|  Workers    v8.11 (ms)    v9.0 (ms)    Change                       |
|  1          4500          3800         -15.6%                       |
|  4          1234          1049         -15.0%                       |
|  8           680           578         -15.0%                       |
|                                                                     |
+---------------------------------------------------------------------+
```

#### View 3: Impact Distribution Chart

Histogram answering "how widespread was the improvement?"

```
# of benchmarks
     |
  15 |     ####
  10 |   ########
   5 | ############
   0 +-----+----+----+----+----+---->
    -20%  -10%   0%  +10%  +20%
         Change in execution time
```

#### View 4: Comparison Summary Table

Sortable details for all benchmarks:

| Benchmark   | Suite    | Before (ms) | After (ms) | Change   | Significant? |
|-------------|----------|-------------|------------|----------|--------------|
| histogram   | MPL      | 1234        | 1049       | -15.0%   | yes          |
| bfs         | MPL      | 890         | 801        | -10.0%   | yes          |
| fib         | MPL      | 456         | 433        | -5.0%    |              |
| b-trees     | Shootout | 234         | 246        | +5.1%    |              |

**Sort by:** Change %, absolute time, suite, name

### Part 4: Technology Stack

**Primary:** Chart.js (existing)
- Horizontal bar chart for butterfly comparison view
- Box plot plugin (already included) for distribution comparison
- Bar chart for impact distribution histogram

**Secondary:** CSS Grid + native HTML for summary cards and tables

---

## Creative Visualization Ideas

### Idea 1: "Performance Diff" View

Style the comparison like a code diff - familiar to developers:

```
  histogram.rkt
- before: 1234 ms (+/-42)
+ after:  1049 ms (+/-25)  down 15% improvement
```

Green/red coloring like git diff, collapsible by suite.

### Idea 2: Benchmark "Weather Report"

At-a-glance summary using weather metaphors:
- Clear improvement (>10% faster across most benchmarks)
- Mostly better (majority improved)
- Mixed results (some better, some worse)
- Some slowdowns detected

### Idea 3: Speedometer/Gauge Cards

Per-benchmark mini-gauges showing relative change as visual indicators.

### Idea 4: Rank Change Visualization

Show how benchmark rankings shifted between versions:

```
v8.11 -> v9.0
#1 histogram  -> #1 histogram (=)
#2 fib        -> #3 fib       (down 2)
#3 bfs        -> #2 bfs       (up 1)
```

---

## Alternative Approaches

### Option A: Static PNG Generation (Simpler)

Use Racket's `plot` library for version comparison charts.
- Pro: Pure Racket, no JavaScript
- Con: Not interactive, limited exploration

### Option B: Vega-Lite Declarative Approach

Generate Vega-Lite JSON specs, render in browser.
- Pro: Declarative grammar, easy to modify
- Con: Different from existing infrastructure

### Option C: Notebook-Based (Jupyter/Observable)

Interactive data exploration notebooks.
- Pro: Flexible analysis, sharable
- Con: Requires additional tooling

---

## Implementation Phases

### Phase 1: Core Infrastructure

1. Create `multi-version-runner.rkt` (orchestration)
2. Extend `analysis.rkt` to extract `racket-version`
3. Add version-aware summary loading

### Phase 2: Basic Visualization

1. Create `version-comparison.html` template
2. Implement butterfly chart (horizontal bar chart showing % change)
3. Summary counts panel (improved/unchanged/slower)
4. Version selector dropdowns (before/after)

### Phase 3: Drill-Down Views

1. Benchmark detail with before/after statistics cards
2. Side-by-side box plots for distribution comparison
3. Worker scaling comparison table
4. Statistical significance indicators

### Phase 4: Integration

1. Add `./bench --compare-versions` CLI command
2. Impact distribution histogram
3. Sortable comparison table
4. Documentation

---

## Key Files

**Create:**
- `benchmarks/tools/multi-version-runner.rkt`
- `benchmarks/tools/version-compare.rkt`
- `benchmarks/tools/templates/version-comparison.html`

**Modify:**
- `benchmarks/tools/analysis.rkt` - Add version extraction
- `bench` - Add `--compare-versions` mode

---

## Verification Plan

1. Run benchmarks against 2-3 Racket versions (e.g., 8.10, 8.11, current)
2. Generate comparison dashboard
3. Test version selector switching between different pairs
4. Verify butterfly chart correctly shows improvements vs. regressions
5. Test drill-down to benchmark detail view
6. Verify summary counts (improved/unchanged/slower) are accurate
7. Validate statistical significance calculations against known data
