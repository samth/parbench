#lang racket

;; Port of tokens from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/tokens
;; Adapted for Racket parallel benchmarking
;;
;; Tokenization: Split text into tokens based on whitespace delimiters.
;; Parallel version uses chunking and parallel processing.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt")

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

(define (compute-token-chunk text start end total-length)
  (define tokens '())
  (for ([idx (in-range start end)])
    (define ch (string-ref text idx))
    (when (and (not (whitespace? ch))
               (or (= idx 0)
                   (whitespace? (string-ref text (sub1 idx)))))
      (define end-index
        (let loop ([pos idx])
          (define next-pos (add1 pos))
          (cond
            [(>= next-pos total-length) total-length]
            [(whitespace? (string-ref text next-pos)) next-pos]
            [else (loop next-pos)])))
      (set! tokens (cons (substring text idx end-index) tokens))))
  (reverse tokens))

;; Parallel tokenization using chunking and thread pools
(define (tokens-parallel text workers)
  (define len (string-length text))
  (cond
    [(or (<= workers 1) (zero? len))
     (tokens-sequential text)]
    [else
     (let* ([task-count (max 1 (min workers len))]
            [chunk-size (max 1 (ceiling (/ len task-count)))]
            [channels
             (for/list ([start (in-range 0 len chunk-size)])
               (define end (min len (+ start chunk-size)))
               (define ch (make-channel))
               (thread (λ () (channel-put ch (compute-token-chunk text start end len))))
               ch)]
            [chunk-token-lists (map channel-get channels)])
       (apply append chunk-token-lists))]))

(module+ main
  (define size 1000000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")
  (define skip-sequential #f)

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
    (set! log-path arg)]
   [("--skip-sequential") "Skip sequential variant"
    (set! skip-sequential #t)])

  ;; Generate input text
  (printf "Generating text with ~a words...\n" size)
  (define text (generate-text size seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'size size)
                       (list 'seed seed)
                       (list 'workers workers)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential tokens(size=~a)...\n" size)
    (set! seq-result
      (run-benchmark
       (λ () (tokens-sequential text))
       #:name 'tokens
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

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
               (when (and seq-result (not (= (length seq-result) (length result))))
                 (error 'tokens "parallel result mismatch at iteration ~a: expected ~a tokens, got ~a"
                        iteration (length seq-result) (length result))))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (= (length seq-result) (length par-result))
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "Tokenized into ~a tokens\n" (length par-result)))
