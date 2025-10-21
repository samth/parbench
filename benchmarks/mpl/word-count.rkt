#lang racket

;; Port of wc from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/wc
;; Adapted for Racket parallel benchmarking
;;
;; Word Count: Count lines, words, and characters in text.
;; Classic MapReduce example with parallel reduction.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide word-count-sequential
         word-count-parallel
         generate-text)

;; Generate random text for benchmarking
(define (generate-text size seed)
  (define words '("the" "quick" "brown" "fox" "jumps" "over" "lazy" "dog"
                  "hello" "world" "racket" "parallel" "benchmark" "test"
                  "data" "processing" "algorithm" "performance"))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (apply string-append
           (for/list ([i (in-range size)])
             (define word (list-ref words (random (length words))))
             (if (= (modulo i 10) 0)
                 (string-append word "\n")  ; Add newlines occasionally
                 (string-append word " "))))))

;; Check if character is whitespace
(define (whitespace? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)
      (char=? c #\return)))

;; Sequential word count
(define (word-count-sequential text)
  (define n (string-length text))
  (define lines 0)
  (define words 0)
  (define in-word? #f)

  (for ([i (in-range n)])
    (define c (string-ref text i))
    (cond
      [(char=? c #\newline)
       (set! lines (add1 lines))
       (when in-word?
         (set! words (add1 words))
         (set! in-word? #f))]
      [(whitespace? c)
       (when in-word?
         (set! words (add1 words))
         (set! in-word? #f))]
      [else
       (set! in-word? #t)]))

  ;; Count last word if exists
  (when in-word?
    (set! words (add1 words)))

  (list lines words n))

;; Parallel word count using reduction
;; NOTE: Proper parallel word count requires careful handling of word boundaries
;; For now, using sequential to ensure correctness
(define (word-count-parallel text workers)
  ;; TODO: Implement true parallel word count with proper boundary handling
  ;; For now, just use sequential to ensure correctness
  (word-count-sequential text))

(module+ main
  (define size 1000000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "word-count"
   #:once-each
   [("--size") arg "Number of words to generate (default: 1000000)"
    (set! size (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)])

  ;; Generate input text
  (printf "Generating text with ~a words...\n" size)
  (define text (generate-text size seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'size size)
                       (list 'seed seed)
                       (list 'workers workers)))

  (printf "Running sequential word-count(size=~a)...\n" size)
  (define seq-result
    (run-benchmark
     (λ () (word-count-sequential text))
     #:name 'word-count
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel word-count(size=~a) (workers=~a)...\n" size workers)
  (define par-result
    (run-benchmark
     (λ () (word-count-parallel text workers))
     #:name 'word-count
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (equal? seq-result result)
                 (error 'word-count "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (equal? seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (match-define (list lines words bytes) seq-result)
  (printf "Lines: ~a, Words: ~a, Bytes: ~a\n" lines words bytes))
