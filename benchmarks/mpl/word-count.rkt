#lang racket

;; Port of wc from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/wc
;; Adapted for Racket parallel benchmarking
;;
;; Word Count: Count lines, words, and characters in text.
;; Classic MapReduce example with parallel reduction.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt")

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

(struct wc-chunk (lines word-starts) #:transparent)

(define (compute-wc-chunk text start end)
  (define lines 0)
  (define word-starts 0)
  (for ([idx (in-range start end)])
    (define ch (string-ref text idx))
    (when (char=? ch #\newline)
      (set! lines (add1 lines)))
    (when (and (not (whitespace? ch))
               (or (= idx 0)
                   (whitespace? (string-ref text (sub1 idx)))))
      (set! word-starts (add1 word-starts))))
  (wc-chunk lines word-starts))

;; Parallel word count using chunk analysis with thread pools
(define (word-count-parallel text workers)
  (define len (string-length text))
  (cond
    [(or (<= workers 1) (zero? len))
     (word-count-sequential text)]
    [else
     (define pool (make-parallel-thread-pool workers))
     (define result
       (let* ([task-count (max 1 (min workers len))]
              [chunk-size (max 1 (ceiling (/ len task-count)))]
              [channels
               (for/list ([start (in-range 0 len chunk-size)])
                 (define end (min len (+ start chunk-size)))
                 (define ch (make-channel))
                 (thread #:pool pool (λ () (channel-put ch (compute-wc-chunk text start end))))
                 ch)]
              [summaries (map channel-get channels)])
         (define total-lines 0)
         (define total-words 0)
         (for ([summary (in-list summaries)])
           (set! total-lines (+ total-lines (wc-chunk-lines summary)))
           (set! total-words (+ total-words (wc-chunk-word-starts summary))))
         (list total-lines total-words len)))
     (parallel-thread-pool-close pool)
     result]))

(module+ main
  (define size 1000000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")
  (define skip-sequential #f)

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
    (printf "Running sequential word-count(size=~a)...\n" size)
    (set! seq-result
      (run-benchmark
       (λ () (word-count-sequential text))
       #:name 'word-count
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

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
               (when (and seq-result (not (equal? seq-result result)))
                 (error 'word-count "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (equal? seq-result par-result)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (match-define (list lines words bytes) par-result)
  (printf "Lines: ~a, Words: ~a, Bytes: ~a\n" lines words bytes))
