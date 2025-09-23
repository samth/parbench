#lang racket

(require racket/system
         racket/string
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(module+ main
  (define binary-path #f)
  (define label #f)
  (define args '())
  (define working-directory #f)
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "nas/run.rkt"
    #:once-each
    [("--binary") path "Path to NAS benchmark executable"
     (set! binary-path path)]
    [("--label") value "Label for the benchmark variant"
     (set! label (string->symbol value))]
    [("--cwd") path "Working directory for the executable"
     (set! working-directory path)]
    [("--repeat") value "Number of invocations"
     (set! repeat (parse-positive-integer value 'nas-run))]
    [("--log") value "Optional S-expression log path"
     (set! log-path value)]
    #:multi
    [("--arg") value "Additional argument (repeatable)"
     (set! args (append args (list value)))]
    #:args positional
    (set! args (append args positional))))

  (unless binary-path
    (error 'nas-run "--binary is required"))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
(define params (list (list 'binary binary-path)
                       (list 'args args)))
(define variant
  (or label
      (string->symbol
       (path->string (file-name-from-path (string->path binary-path))))))

  (define (invoke)
    (parameterize ([current-directory (or working-directory (current-directory))])
      (define result (apply system* binary-path args))
      (unless result
        (error 'nas-run "NAS binary exited with failure"))
      result))

  (run-benchmark
   invoke
   #:name 'nas
   #:variant variant
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata)

  (close-log-writer writer))
