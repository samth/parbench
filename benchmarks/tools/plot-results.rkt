#lang racket

(require racket/cmdline
         racket/list
         racket/math
         racket/string
         plot
         "analysis.rkt")

(module+ main
  (define inputs '())
  (define output-path #f)
  (define metric 'real)
  (define title "Benchmark Results")

  (void
   (command-line
    #:program "plot-results.rkt"
    #:once-each
    [("--input") path "Benchmark log file (repeatable)."
     (set! inputs (cons path inputs))]
    [("--output") path "PNG path for generated plot."
     (set! output-path path)]
    [("--metric") value "Metric to plot: real or cpu."
     (define normalized (string->symbol (string-downcase value)))
     (unless (memq normalized '(real cpu))
       (error 'plot-results "unknown metric ~a" value))
     (set! metric normalized)]
    [("--title") value "Plot title."
     (set! title value)]
    #:args positional
    (set! inputs (append inputs positional))))

  (unless output-path
    (error 'plot-results "--output is required"))

  (define summaries (load-summaries (reverse inputs)))
  (when (null? summaries)
    (error 'plot-results "no benchmark records found"))

  (define names
    (sort (remove-duplicates (map summary-name summaries))
          string<? #:key symbol->string))
  (define name->index
    (for/hash ([name names] [idx (in-naturals)])
      (values name idx)))
  (define variants
    (sort (remove-duplicates (map summary-variant summaries))
          string<? #:key symbol->string))
  (define variant-count (max 1 (length variants)))
  (define bar-width (/ 0.8 variant-count))
  (define offsets
    (for/list ([i (in-naturals 0 variant-count)])
      (- (* i bar-width)
         (* bar-width (/ (sub1 variant-count) 2.0)))))
  (define metric-label (if (eq? metric 'cpu) "cpu-mean(ms)" "real-mean(ms)"))
  (define metric-fn (if (eq? metric 'cpu) summary-cpu-mean summary-real-mean))

  (define datasets
    (for/list ([variant variants] [offset offsets])
      (define entries (filter (λ (s) (eq? (summary-variant s) variant)) summaries))
      (and (pair? entries)
           (bars (for/list ([s entries])
                   (define base (hash-ref name->index (summary-name s)))
                   (vector (+ base offset) (metric-fn s)))
                 #:width bar-width
                 #:label (symbol->string variant)))))

  (define active-datasets (filter identity datasets))
  (unless (pair? active-datasets)
    (error 'plot-results "no datasets matched the selection"))

  (define ticks
    (for/list ([name names] [idx (in-naturals)])
      (list idx (symbol->string name))))

  (plot active-datasets
        #:x-label "benchmark"
        #:y-label metric-label
        #:title title
        #:x-ticks ticks
        #:x-min -0.5
        #:x-max (+ (length names) -0.5)
        #:out-kind 'png
        #:out-file output-path))
