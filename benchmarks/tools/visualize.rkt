#lang racket

(require racket/cmdline
         racket/path
         racket/file
         racket/system
         racket/list
         racket/format
         json
         "analysis.rkt")

(provide run-and-visualize)

;; Generate JSON data from benchmark summaries
(define (summaries->json summaries)
  (define grouped (group-by summary-name summaries))
  (hasheq
   'benchmarks
   (for/list ([group grouped])
     (define bench-name (summary-name (first group)))
     (hasheq
      'name (symbol->string bench-name)
      'variants
      (for/list ([s group])
        (hasheq
         'variant (symbol->string (summary-variant s))
         'count (summary-count s)
         'real_mean (summary-real-mean s)
         'real_stddev (summary-real-stddev s)
         'real_min (summary-real-min s)
         'real_max (summary-real-max s)
         'cpu_mean (summary-cpu-mean s)))))))

;; HTML template with embedded JavaScript for interactive visualization
(define (generate-html json-data title)
  (format #<<HTML
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>~a</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 2rem;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            border-radius: 16px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            overflow: hidden;
        }

        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 2rem;
            text-align: center;
        }

        .header h1 {
            font-size: 2.5rem;
            margin-bottom: 0.5rem;
            font-weight: 700;
        }

        .header p {
            font-size: 1.1rem;
            opacity: 0.9;
        }

        .controls {
            display: flex;
            gap: 1rem;
            padding: 1.5rem 2rem;
            background: #f8f9fa;
            border-bottom: 1px solid #e9ecef;
            flex-wrap: wrap;
            align-items: center;
        }

        .control-group {
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }

        .control-group label {
            font-weight: 600;
            color: #495057;
            font-size: 0.9rem;
        }

        .control-group select {
            padding: 0.5rem 1rem;
            border: 2px solid #dee2e6;
            border-radius: 8px;
            font-size: 0.9rem;
            cursor: pointer;
            background: white;
            transition: all 0.2s;
        }

        .control-group select:hover {
            border-color: #667eea;
        }

        .control-group select:focus {
            outline: none;
            border-color: #667eea;
            box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
        }

        .content {
            padding: 2rem;
        }

        .chart-container {
            position: relative;
            height: 500px;
            margin-bottom: 2rem;
        }

        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 1rem;
            margin-bottom: 2rem;
        }

        .stat-card {
            background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
            padding: 1.5rem;
            border-radius: 12px;
            border: 2px solid #dee2e6;
            transition: all 0.3s;
        }

        .stat-card:hover {
            transform: translateY(-4px);
            box-shadow: 0 8px 16px rgba(0,0,0,0.1);
            border-color: #667eea;
        }

        .stat-card h3 {
            color: #495057;
            font-size: 0.9rem;
            margin-bottom: 0.5rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .stat-card .value {
            font-size: 2rem;
            font-weight: 700;
            color: #667eea;
        }

        .stat-card .unit {
            font-size: 0.9rem;
            color: #6c757d;
            margin-left: 0.25rem;
        }

        .details-table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 1rem;
            background: white;
            border-radius: 8px;
            overflow: hidden;
            box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }

        .details-table th {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 1rem;
            text-align: left;
            font-weight: 600;
            text-transform: uppercase;
            font-size: 0.85rem;
            letter-spacing: 0.5px;
        }

        .details-table td {
            padding: 1rem;
            border-bottom: 1px solid #e9ecef;
        }

        .details-table tbody tr:hover {
            background: #f8f9fa;
        }

        .details-table tbody tr:last-child td {
            border-bottom: none;
        }

        .benchmark-name {
            font-weight: 600;
            color: #212529;
        }

        .variant-name {
            display: inline-block;
            padding: 0.25rem 0.75rem;
            background: #e7f1ff;
            color: #0c63e4;
            border-radius: 16px;
            font-size: 0.85rem;
            font-weight: 500;
        }

        .metric-value {
            font-family: 'Courier New', monospace;
            color: #495057;
        }

        footer {
            text-align: center;
            padding: 1.5rem;
            background: #f8f9fa;
            color: #6c757d;
            font-size: 0.9rem;
            border-top: 1px solid #e9ecef;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>~a</h1>
            <p>Interactive Benchmark Analysis Dashboard</p>
        </div>

        <div class="controls">
            <div class="control-group">
                <label for="chartType">Chart Type:</label>
                <select id="chartType" onchange="updateChart()">
                    <option value="bar">Bar Chart</option>
                    <option value="line">Line Chart</option>
                    <option value="radar">Radar Chart</option>
                </select>
            </div>

            <div class="control-group">
                <label for="metricType">Metric:</label>
                <select id="metricType" onchange="updateChart()">
                    <option value="real_mean">Real Time (Mean)</option>
                    <option value="cpu_mean">CPU Time (Mean)</option>
                    <option value="real_min">Real Time (Min)</option>
                    <option value="real_max">Real Time (Max)</option>
                </select>
            </div>

            <div class="control-group">
                <label for="scaleType">Scale:</label>
                <select id="scaleType" onchange="updateChart()">
                    <option value="linear">Linear</option>
                    <option value="logarithmic">Logarithmic</option>
                </select>
            </div>
        </div>

        <div class="content">
            <div class="stats-grid" id="statsGrid"></div>

            <div class="chart-container">
                <canvas id="benchmarkChart"></canvas>
            </div>

            <h2 style="margin-bottom: 1rem; color: #212529;">Detailed Results</h2>
            <table class="details-table" id="detailsTable">
                <thead>
                    <tr>
                        <th>Benchmark</th>
                        <th>Variant</th>
                        <th>Count</th>
                        <th>Real Mean (ms)</th>
                        <th>Real StdDev (ms)</th>
                        <th>Real Min (ms)</th>
                        <th>Real Max (ms)</th>
                        <th>CPU Mean (ms)</th>
                    </tr>
                </thead>
                <tbody></tbody>
            </table>
        </div>

        <footer>
            Generated on <span id="timestamp"></span> | Data points: <span id="dataPoints"></span>
        </footer>
    </div>

    <script>
        const data = ~a;
        let chart = null;

        function updateStats() {
            const statsGrid = document.getElementById('statsGrid');
            let totalBenchmarks = data.benchmarks.length;
            let totalVariants = 0;
            let totalRuns = 0;
            let avgRealTime = 0;
            let count = 0;

            data.benchmarks.forEach(bench => {
                bench.variants.forEach(variant => {
                    totalVariants++;
                    totalRuns += variant.count;
                    avgRealTime += variant.real_mean;
                    count++;
                });
            });

            avgRealTime = count > 0 ? (avgRealTime / count).toFixed(2) : 0;

            statsGrid.innerHTML = `
                <div class="stat-card">
                    <h3>Total Benchmarks</h3>
                    <div class="value">$${totalBenchmarks}</div>
                </div>
                <div class="stat-card">
                    <h3>Total Variants</h3>
                    <div class="value">$${totalVariants}</div>
                </div>
                <div class="stat-card">
                    <h3>Total Runs</h3>
                    <div class="value">$${totalRuns}</div>
                </div>
                <div class="stat-card">
                    <h3>Avg Real Time</h3>
                    <div class="value">$${avgRealTime}<span class="unit">ms</span></div>
                </div>
            `;
        }

        function updateTable() {
            const tbody = document.querySelector('#detailsTable tbody');
            tbody.innerHTML = '';

            data.benchmarks.forEach(bench => {
                bench.variants.forEach(variant => {
                    const row = tbody.insertRow();
                    row.innerHTML = `
                        <td class="benchmark-name">$${bench.name}</td>
                        <td><span class="variant-name">$${variant.variant}</span></td>
                        <td class="metric-value">$${variant.count}</td>
                        <td class="metric-value">$${variant.real_mean.toFixed(2)}</td>
                        <td class="metric-value">$${variant.real_stddev.toFixed(2)}</td>
                        <td class="metric-value">$${variant.real_min.toFixed(2)}</td>
                        <td class="metric-value">$${variant.real_max.toFixed(2)}</td>
                        <td class="metric-value">$${variant.cpu_mean.toFixed(2)}</td>
                    `;
                });
            });
        }

        function updateChart() {
            const chartType = document.getElementById('chartType').value;
            const metricType = document.getElementById('metricType').value;
            const scaleType = document.getElementById('scaleType').value;

            const labels = [];
            const datasets = {};

            // Collect all unique variants
            const variantSet = new Set();
            data.benchmarks.forEach(bench => {
                bench.variants.forEach(variant => {
                    variantSet.add(variant.variant);
                });
            });

            const variants = Array.from(variantSet);
            const colors = [
                'rgba(102, 126, 234, 0.8)',
                'rgba(118, 75, 162, 0.8)',
                'rgba(237, 100, 166, 0.8)',
                'rgba(255, 154, 102, 0.8)',
                'rgba(52, 211, 153, 0.8)',
                'rgba(59, 130, 246, 0.8)',
            ];

            // Initialize datasets for each variant
            variants.forEach((variant, idx) => {
                datasets[variant] = {
                    label: variant,
                    data: [],
                    backgroundColor: colors[idx % colors.length],
                    borderColor: colors[idx % colors.length].replace('0.8', '1'),
                    borderWidth: 2,
                };
            });

            // Populate data
            data.benchmarks.forEach(bench => {
                labels.push(bench.name);
                const variantMap = {};
                bench.variants.forEach(v => {
                    variantMap[v.variant] = v[metricType];
                });

                variants.forEach(variant => {
                    datasets[variant].data.push(variantMap[variant] || 0);
                });
            });

            if (chart) {
                chart.destroy();
            }

            const ctx = document.getElementById('benchmarkChart').getContext('2d');
            chart = new Chart(ctx, {
                type: chartType,
                data: {
                    labels: labels,
                    datasets: Object.values(datasets)
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        legend: {
                            display: true,
                            position: 'top',
                        },
                        title: {
                            display: true,
                            text: 'Benchmark Performance Comparison',
                            font: {
                                size: 16,
                                weight: 'bold'
                            }
                        },
                        tooltip: {
                            callbacks: {
                                label: function(context) {
                                    return context.dataset.label + ': ' + context.parsed.y.toFixed(2) + ' ms';
                                }
                            }
                        }
                    },
                    scales: chartType !== 'radar' ? {
                        y: {
                            type: scaleType,
                            beginAtZero: true,
                            title: {
                                display: true,
                                text: 'Time (ms)'
                            }
                        },
                        x: {
                            title: {
                                display: true,
                                text: 'Benchmark'
                            }
                        }
                    } : {}
                }
            });
        }

        // Initialize
        document.getElementById('timestamp').textContent = new Date().toLocaleString();
        let dataPoints = 0;
        data.benchmarks.forEach(bench => {
            bench.variants.forEach(variant => {
                dataPoints += variant.count;
            });
        });
        document.getElementById('dataPoints').textContent = dataPoints;

        updateStats();
        updateTable();
        updateChart();
    </script>
</body>
</html>
HTML
          title
          title
          json-data))

;; Main visualization function
(define (run-and-visualize #:suite-names [suite-names '("all")]
                            #:config-path [config-path #f]
                            #:log-dir [log-dir "logs"]
                            #:output [output "benchmark-results.html"]
                            #:title [title "Benchmark Results"]
                            #:run-benchmarks? [run-benchmarks? #t])

  (when run-benchmarks?
    (printf "Running benchmarks...\n")
    (define run-suite-path (build-path (current-directory) "benchmarks" "run-suite.rkt"))
    (define args (append (list (path->string run-suite-path))
                        (apply append (for/list ([suite suite-names])
                                       (list "--suite" suite)))
                        (list "--log-dir" log-dir)
                        (if config-path
                            (list "--config" config-path)
                            '())))

    (define-values (proc out in err)
      (apply subprocess #f #f #f (find-executable-path "racket") args))
    (subprocess-wait proc)
    (close-output-port in)
    (close-input-port out)
    (close-input-port err)

    (define exit-code (subprocess-status proc))
    (unless (zero? exit-code)
      (error 'run-and-visualize "Benchmark suite failed with exit code: ~a" exit-code)))

  (printf "\nGenerating visualization...\n")

  ;; Collect all .sexp files from log directory
  (define log-files
    (if (directory-exists? log-dir)
        (for/list ([file (directory-list log-dir)]
                   #:when (regexp-match? #rx"\\.sexp$" (path->string file)))
          (build-path log-dir file))
        '()))

  (when (null? log-files)
    (error 'run-and-visualize "No .sexp log files found in ~a" log-dir))

  (printf "Found ~a log file(s)\n" (length log-files))

  ;; Load and aggregate results
  (define summaries (load-summaries (map path->string log-files)))

  (when (null? summaries)
    (error 'run-and-visualize "No benchmark data found in log files"))

  (printf "Loaded ~a benchmark result(s)\n" (length summaries))

  ;; Generate JSON
  (define json-hash (summaries->json summaries))
  (define json-string (jsexpr->string json-hash))

  ;; Generate HTML
  (define html (generate-html json-string title))

  ;; Write output file
  (call-with-output-file output
    #:exists 'replace
    (λ (out) (display html out)))

  (printf "✓ Visualization saved to: ~a\n" output)
  (printf "\nOpen the file in a web browser to view the interactive dashboard.\n"))

(module+ main
  (define suite-names '())
  (define config-path #f)
  (define log-dir "logs")
  (define output "benchmark-results.html")
  (define title "Benchmark Results")
  (define run-benchmarks? #t)

  (void
   (command-line
    #:program "visualize.rkt"
    #:once-each
    [("--suite" "-s") suite "Suite to run: racket, shootout, nas, mpl, or all"
     (set! suite-names (cons suite suite-names))]
    [("--config" "-c") path "Configuration file (S-expression)"
     (set! config-path path)]
    [("--log-dir" "-l") dir "Directory for log files (default: logs)"
     (set! log-dir dir)]
    [("--output" "-o") file "Output HTML file (default: benchmark-results.html)"
     (set! output file)]
    [("--title" "-t") t "Dashboard title (default: Benchmark Results)"
     (set! title t)]
    [("--no-run") "Skip running benchmarks, only generate visualization from existing logs"
     (set! run-benchmarks? #f)]
    #:args ()
    (void)))

  (when (null? suite-names)
    (set! suite-names '("all")))

  (run-and-visualize #:suite-names (reverse suite-names)
                     #:config-path config-path
                     #:log-dir log-dir
                     #:output output
                     #:title title
                     #:run-benchmarks? run-benchmarks?))
