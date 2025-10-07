#lang racket

(require racket/cmdline
         "analysis.rkt")

(module+ main
  (define inputs '())

  (void
   (command-line
    #:program "summarize-results.rkt"
    #:once-each
    [("--file") path "Benchmark log file (S-expression format)."
     (set! inputs (cons path inputs))]
    #:args positional
    (set! inputs (append inputs positional))))

  (define summaries (load-summaries (reverse inputs)))
  (define sorted
    (sort summaries
          (λ (a b)
            (string<? (symbol->string (summary-name a))
                      (symbol->string (summary-name b))))))

  (printf "name variant count real-mean(ms) real-stddev real-min real-max cpu-mean(ms)\n")
  (for ([s sorted])
    (printf "~a ~a ~a ~a ~a ~a ~a ~a\n"
            (summary-name s)
            (summary-variant s)
            (summary-count s)
            (real->decimal-string (summary-real-mean s) 3)
            (real->decimal-string (summary-real-stddev s) 3)
            (real->decimal-string (summary-real-min s) 3)
            (real->decimal-string (summary-real-max s) 3)
            (real->decimal-string (summary-cpu-mean s) 3))))
