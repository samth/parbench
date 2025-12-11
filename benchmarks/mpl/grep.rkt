#lang racket

;; Port of grep from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/grep
;; Adapted for Racket parallel benchmarking
;;
;; Grep: Search for lines matching a regular expression pattern in text.
;; Parallel version processes chunks of lines concurrently.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt")

(provide grep-sequential
         grep-parallel
         generate-random-text)

;; Generate random text with some lines containing pattern
(define (generate-random-text num-lines pattern-freq seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (define words '("the" "quick" "brown" "fox" "jumps" "over" "lazy" "dog"
                    "hello" "world" "racket" "scheme" "lisp" "functional" "programming"))

    (for/list ([i (in-range num-lines)])
      (define has-pattern? (< (random) pattern-freq))
      (define line-words
        (if has-pattern?
            (cons "PATTERN"
                  (for/list ([j (in-range (+ 3 (random 10)))])
                    (list-ref words (random (length words)))))
            (for/list ([j (in-range (+ 3 (random 10)))])
              (list-ref words (random (length words))))))
      (string-join line-words " "))))

;; Sequential grep: return indices of matching lines
(define (grep-sequential lines pattern)
  (define rx (regexp pattern))
  (for/list ([line (in-list lines)]
             [idx (in-naturals)]
             #:when (regexp-match? rx line))
    idx))

;; Parallel grep: process chunks in parallel
(define (grep-parallel lines pattern workers)
  (define rx (regexp pattern))
  (define n (length lines))
  (define (process-range start end)
    (define chunk (take (drop lines start) (- end start)))
    (for/list ([line (in-list chunk)]
               [local-idx (in-naturals)]
               #:when (regexp-match? rx line))
      (+ start local-idx)))

  (if (<= workers 1)
      (grep-sequential lines pattern)
      (let* ([effective-workers (max 1 (min workers n))]
             [chunk-size (max 1 (ceiling (/ n effective-workers)))]
             [channels
              (for/list ([start (in-range 0 n chunk-size)])
                (define end (min n (+ start chunk-size)))
                (define ch (make-channel))
                (thread (λ () (channel-put ch (process-range start end))))
                ch)]
             [results (map channel-get channels)])
        (sort (apply append results) <))))

(module+ main
  (define num-lines 10000)
  (define pattern "PATTERN")
  (define pattern-freq 0.1)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")
  (define skip-sequential #f)

  (command-line
   #:program "grep"
   #:once-each
   [("--lines") arg "Number of lines (default: 10000)"
    (set! num-lines (string->number arg))]
   [("--pattern") arg "Pattern to search (default: 'PATTERN')"
    (set! pattern arg)]
   [("--freq") arg "Frequency of pattern in text (default: 0.1)"
    (set! pattern-freq (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)]
   [("--skip-sequential") "Skip sequential variant"
    (set! skip-sequential #t)])

  ;; Generate input text
  (printf "Generating random text with ~a lines...\\n" num-lines)
  (define text (generate-random-text num-lines pattern-freq seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'num-lines num-lines)
                       (list 'pattern pattern)
                       (list 'pattern-freq pattern-freq)
                       (list 'seed seed)
                       (list 'workers workers)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential grep(lines=~a, pattern=~a)...\\n" num-lines pattern)
    (set! seq-result
      (run-benchmark
       (λ () (grep-sequential text pattern))
       #:name 'grep
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel grep(lines=~a, pattern=~a) (workers=~a)...\\n" num-lines pattern workers)
  (define par-result
    (run-benchmark
     (λ () (grep-parallel text pattern workers))
     #:name 'grep
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (equal? seq-result result)))
                 (error 'grep "parallel result mismatch at iteration ~a"
                        iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\\nVerification: ")
    (if (equal? seq-result par-result)
        (printf "✓ Sequential and parallel results match\\n")
        (printf "✗ Results differ!\\n")))

  (printf "Found ~a matching lines\\n" (length par-result)))
