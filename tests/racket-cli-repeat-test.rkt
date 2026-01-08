#lang racket

(require rackunit
         racket/file
         racket/runtime-path)

(define-runtime-path test-dir ".")

(define (run-benchmark-main relative-path args)
  (define tmp (make-temporary-file "benchmark-log~a.sexp"))
  (define module-path (simplify-path (build-path test-dir ".." relative-path)))
  (dynamic-wind
    void
    (λ ()
      (parameterize ([current-command-line-arguments
                      (list->vector (append args (list "--log" (path->string tmp))))]
                     [current-output-port (open-output-bytes)]
                     [current-error-port (open-output-bytes)])
        (dynamic-require `(submod ,module-path main) #f)))
    (λ ()
      (when (file-exists? tmp) (delete-file tmp)))))

(test-case "bmbench accepts repeat override"
  (check-not-exn
   (λ ()
     (run-benchmark-main
      "benchmarks/racket/bmbench.rkt"
      '("--n" "64"
        "--workers" "1"
        "--repeat" "2")))))

(test-case "richards accepts repeat override"
  (check-not-exn
   (λ ()
     (run-benchmark-main
      "benchmarks/racket/richards.rkt"
      '("--iterations" "2"
        "--workers" "1"
        "--repeat" "2")))))
