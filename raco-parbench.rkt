#lang racket/base

;; raco parbench - wrapper for the bench script
;; This module allows running `raco parbench` with the same arguments as `./bench`

(require racket/runtime-path
         racket/system
         racket/vector)

(define-runtime-path bench-script "bench")

(module+ main
  (define args (current-command-line-arguments))
  (define cmd (cons (path->string bench-script) (vector->list args)))
  (exit (apply system*/exit-code (find-executable-path "racket") cmd)))
