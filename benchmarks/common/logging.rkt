#lang racket

(require racket/format
         racket/path)

(provide make-log-writer
         close-log-writer
         log-event
         system-metadata)

(struct log-writer (out) #:transparent)

(define (make-log-writer path)
  (cond
    [(or (not path) (string=? path "")) (log-writer #f)]
    [else
     (define out-path (string->path path))
     (define dir (path-only out-path))
     (when dir (make-directory* dir))
     (log-writer (open-output-file out-path #:exists 'replace))]))

(define (close-log-writer writer)
  (when (and writer (log-writer-out writer))
    (close-output-port (log-writer-out writer))))

(define (emit-line writer sexpr)
  (define line (format "~s" sexpr))
  (displayln line)
  (flush-output)
  (when (and writer (log-writer-out writer))
    (displayln line (log-writer-out writer))
    (flush-output (log-writer-out writer))))

(define (log-event writer sexpr)
  (emit-line writer sexpr)
  sexpr)

(define (system-metadata)
  (list (cons 'timestamp (current-seconds))
        (cons 'racket-version (version))
        (cons 'system (system-type))
        (cons 'arch (system-type 'arch))
        (cons 'cpu-count (processor-count))))
