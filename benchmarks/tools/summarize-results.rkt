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
  (define (worker->string w)
    (cond
      [(number? w)
       (define s (number->string w))
       (define padding (max 0 (- 8 (string-length s))))
       (string-append (make-string padding #\0) s)]
      [(false? w) "zz"]
      [(string? w) w]
      [(symbol? w) (symbol->string w)]
      [else (format "~a" w)]))

  (define (summary-sort-key s)
    (define name (symbol->string (summary-name s)))
    (define worker-str (worker->string (summary-worker s)))
    (define variant (symbol->string (summary-variant s)))
    (format "~a|~a|~a" name worker-str variant))

  (define sorted
    (sort summaries string<? #:key summary-sort-key))

  (printf "name variant worker count real-mean(ms) real-stddev real-min real-max cpu-mean(ms) gc-mean(ms)\n")
  (for ([s sorted])
    (printf "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a\n"
            (summary-name s)
            (summary-variant s)
            (or (summary-worker s) "-")
            (summary-count s)
            (real->decimal-string (summary-real-mean s) 3)
            (real->decimal-string (summary-real-stddev s) 3)
            (real->decimal-string (summary-real-min s) 3)
            (real->decimal-string (summary-real-max s) 3)
            (real->decimal-string (summary-cpu-mean s) 3)
            (real->decimal-string (summary-gc-mean s) 3))))
