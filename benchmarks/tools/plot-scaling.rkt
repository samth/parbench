#lang racket

(require racket/cmdline
         racket/list
         racket/math
         racket/string
         plot/no-gui)

(provide plot-scaling-from-logs)

;; Extract field from association list
(define (extract-field key pairs)
  (define entry (assoc key pairs))
  (cond
    [(not entry) #f]
    [else
     (define tail (cdr entry))
     (cond
       [(null? tail) #f]
       [(and (pair? tail) (not (pair? (car tail)))) (car tail)]
       [else tail])]))

;; Extract metric from metrics list
(define (extract-metric key metrics)
  (cond
    [(not metrics) #f]
    [(null? metrics) #f]
    [else
     (define entry (assoc key metrics))
     (cond
       [(not entry) #f]
       [(null? (cdr entry)) #f]
       [else (cadr entry)])]))

;; Extract parameter value
(define (extract-param key params)
  (extract-metric key params))

;; Read one datum from port, return #f on read error
(define (read-datum in)
  (with-handlers ([exn:fail:read? (λ (_) #f)])
    (read in)))

;; Load all benchmark records from files
(define (load-benchmarks paths)
  (define results '())
  (define files (if (null? paths) (list "-") paths))
  (for ([path (in-list files)])
    (define records
      (cond
        [(string=? path "-")
         (let loop ([acc '()])
           (define datum (read-datum (current-input-port)))
           (cond
             [(eof-object? datum) (reverse acc)]
             [(and datum (list? datum) (pair? datum) (eq? (car datum) 'benchmark))
              (loop (cons datum acc))]
             [else (loop acc)]))]
        [else
         (call-with-input-file path
           (λ (in)
             (let loop ([acc '()])
               (define datum (read-datum in))
               (cond
                 [(eof-object? datum) (reverse acc)]
                 [(and datum (list? datum) (pair? datum) (eq? (car datum) 'benchmark))
                  (loop (cons datum acc))]
                 [else (loop acc)]))))]))
    (set! results (append results records)))
  results)

;; Parse benchmark record into structured data
(define (parse-benchmark rec)
  (define fields (cdr rec))
  (define name (extract-field 'name fields))
  (define variant (extract-field 'variant fields))
  (define metrics (extract-field 'metrics fields))
  (define params (extract-field 'params fields))
  (define workers (or (extract-param 'workers params) 1))
  (define real-ms (extract-metric 'real-ms metrics))
  (define cpu-ms (extract-metric 'cpu-ms metrics))
  (and name variant real-ms
       (hash 'name name
             'variant variant
             'workers workers
             'real-ms real-ms
             'cpu-ms cpu-ms)))

;; Group benchmarks by name and variant, collect data points
(define (group-by-workers benchmarks)
  (define groups (make-hash))
  (for ([rec (in-list benchmarks)])
    (define parsed (parse-benchmark rec))
    (when parsed
      (define key (cons (hash-ref parsed 'name)
                        (hash-ref parsed 'variant)))
      (define point (cons (hash-ref parsed 'workers)
                          (hash-ref parsed 'real-ms)))
      (hash-update! groups key (λ (pts) (cons point pts)) '())))
  groups)

;; Create scaling plot
(define (plot-scaling-from-logs paths output-path benchmark-name metric title)
  (define records (load-benchmarks paths))
  (when (null? records)
    (error 'plot-scaling "no benchmark records found"))

  (define groups (group-by-workers records))
  (define metric-key (if (eq? metric 'cpu) 'cpu-ms 'real-ms))

  ;; Filter by benchmark name if specified
  (define filtered-groups
    (if benchmark-name
        (for/hash ([(key pts) (in-hash groups)]
                   #:when (eq? (car key) benchmark-name))
          (values key pts))
        groups))

  (when (hash-empty? filtered-groups)
    (error 'plot-scaling "no matching benchmarks found"))

  ;; Get baseline (sequential or 1-worker) performance for speedup calculation
  (define (get-baseline name)
    (define seq-key (cons name 'sequential))
    (define par1-key (cons name 'parallel))
    (define baseline
      (or (and (hash-has-key? filtered-groups seq-key)
               (apply min (map cdr (hash-ref filtered-groups seq-key))))
          (let ([par-pts (hash-ref filtered-groups par1-key '())])
            (define w1-pts (filter (λ (p) (= (car p) 1)) par-pts))
            (and (pair? w1-pts)
                 (apply min (map cdr w1-pts))))))
    baseline)

  ;; Create line renderers for each variant
  (define renderers
    (for/list ([(key pts) (in-hash filtered-groups)]
               #:when (eq? (cdr key) 'parallel))
      (define name (car key))
      (define variant (cdr key))
      (define sorted (sort pts < #:key car))
      (define baseline (get-baseline name))

      ;; Calculate speedup if we have a baseline
      (define data-points
        (if baseline
            (for/list ([pt (in-list sorted)])
              (vector (car pt) (/ baseline (cdr pt))))
            (for/list ([pt (in-list sorted)])
              (vector (car pt) (cdr pt)))))

      (define label (format "~a" name))
      (list (lines data-points #:label label #:width 2)
            (points data-points #:sym 'fullcircle #:size 8))))

  (define flat-renderers (apply append renderers))

  ;; Add ideal scaling line if showing speedup
  (when (get-baseline (caar (hash-keys filtered-groups)))
    (define max-workers
      (apply max (for*/list ([(key pts) (in-hash filtered-groups)]
                             [pt (in-list pts)])
                   (car pt))))
    (set! flat-renderers
          (cons (lines (list (vector 1 1) (vector max-workers max-workers))
                      #:label "ideal"
                      #:color "gray"
                      #:style 'long-dash
                      #:width 1)
                flat-renderers)))

  (define y-label
    (if (get-baseline (caar (hash-keys filtered-groups)))
        "Speedup"
        (if (eq? metric 'cpu) "CPU time (ms)" "Real time (ms)")))

  (plot-file flat-renderers
             output-path
             #:x-label "Workers"
             #:y-label y-label
             #:title title
             #:width 800
             #:height 600))

(module+ main
  (define inputs '())
  (define output-path #f)
  (define benchmark-name #f)
  (define metric 'real)
  (define title "Scaling Performance")

  (void
   (command-line
    #:program "plot-scaling.rkt"
    #:once-each
    [("--input") path "Benchmark log file (repeatable)."
     (set! inputs (cons path inputs))]
    [("--output") path "PNG path for generated plot."
     (set! output-path path)]
    [("--benchmark") name "Filter to specific benchmark name."
     (set! benchmark-name (string->symbol name))]
    [("--metric") value "Metric to plot: real or cpu."
     (define normalized (string->symbol (string-downcase value)))
     (unless (memq normalized '(real cpu))
       (error 'plot-scaling "unknown metric ~a" value))
     (set! metric normalized)]
    [("--title") value "Plot title."
     (set! title value)]
    #:args positional
    (set! inputs (append inputs positional))))

  (unless output-path
    (error 'plot-scaling "--output is required"))

  (plot-scaling-from-logs (reverse inputs) output-path benchmark-name metric title))
