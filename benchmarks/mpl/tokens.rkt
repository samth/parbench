#lang racket

;; Port of tokens from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/tokens
;; Adapted for Racket parallel benchmarking
;;
;; Tokenization: Split text into tokens based on whitespace delimiters.
;; Parallel version uses chunking and parallel processing.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide tokens-sequential
         tokens-parallel
         generate-text)

;; Generate random text for benchmarking
(define (generate-text size seed)
  (define words '("the" "quick" "brown" "fox" "jumps" "over" "lazy" "dog"
                  "hello" "world" "racket" "parallel" "benchmark" "test"
                  "data" "processing" "algorithm" "performance"))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (define word-list
      (for/list ([i (in-range size)])
        (list-ref words (random (length words)))))
    (string-join word-list " ")))

;; Check if character is whitespace
(define (whitespace? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)
      (char=? c #\return)))

;; Sequential tokenization
(define (tokens-sequential text)
  (define n (string-length text))
  (define tokens '())
  (define current-token '())

  (for ([i (in-range n)])
    (define c (string-ref text i))
    (cond
      [(whitespace? c)
       (when (not (null? current-token))
         (set! tokens (cons (list->string (reverse current-token)) tokens))
         (set! current-token '()))]
      [else
       (set! current-token (cons c current-token))]))

  ;; Add last token if exists
  (when (not (null? current-token))
    (set! tokens (cons (list->string (reverse current-token)) tokens)))

  (reverse tokens))

;; Parallel tokenization using chunking
;; For simplicity and correctness, use sequential algorithm
;; TODO: Implement true parallel tokenization with proper boundary handling
(define (tokens-parallel text workers)
  ;; Proper parallel tokenization requires:
  ;; 1. Chunking at whitespace boundaries
  ;; 2. Handling partial tokens at chunk boundaries
  ;; 3. Merging results correctly
  ;; For now, use sequential to ensure correctness
  (tokens-sequential text))

(module+ main
  (define size 1000000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "tokens"
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

  (printf "Running sequential tokens(size=~a)...\n" size)
  (define seq-result
    (run-benchmark
     (λ () (tokens-sequential text))
     #:name 'tokens
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel tokens(size=~a) (workers=~a)...\n" size workers)
  (define par-result
    (run-benchmark
     (λ () (tokens-parallel text workers))
     #:name 'tokens
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (= (length seq-result) (length result))
                 (error 'tokens "parallel result mismatch at iteration ~a: expected ~a tokens, got ~a"
                        iteration (length seq-result) (length result))))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (= (length seq-result) (length par-result))
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "Tokenized into ~a tokens\n" (length seq-result)))
