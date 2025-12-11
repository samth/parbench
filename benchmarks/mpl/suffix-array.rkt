#lang racket

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide suffix-array-sequential
         suffix-array-parallel
         verify-suffix-array)

;; Suffix array: Given a string s, return the lexicographically sorted
;; array of all suffixes (represented as starting positions)

;; Sequential suffix array using prefix-doubling algorithm
(define (suffix-array-sequential text)
  (define n (string-length text))
  (define text-vec (list->vector (string->list text)))

  ;; Initial rank based on first character
  (define rank (make-vector n 0))
  (for ([i (in-range n)])
    (vector-set! rank i (char->integer (vector-ref text-vec i))))

  ;; Create suffix array (initially just positions)
  (define sa (for/vector ([i (in-range n)]) i))

  ;; Prefix doubling: sort by rank[i], then rank[i+k], doubling k each iteration
  (let loop ([k 1])
    (when (< k n)
      ;; Sort suffixes by (rank[i], rank[i+k])
      (define sa-list (vector->list sa))
      (define sorted-sa
        (sort sa-list
              (λ (i j)
                (define ri (vector-ref rank i))
                (define rj (vector-ref rank j))
                (if (= ri rj)
                    (let ([rik (if (< (+ i k) n) (vector-ref rank (+ i k)) -1)]
                          [rjk (if (< (+ j k) n) (vector-ref rank (+ j k)) -1)])
                      (< rik rjk))
                    (< ri rj)))))
      (set! sa (list->vector sorted-sa))

      ;; Update ranks based on new order
      (define new-rank (make-vector n 0))
      (vector-set! new-rank (vector-ref sa 0) 0)
      (for ([idx (in-range 1 n)])
        (define i (vector-ref sa idx))
        (define prev-i (vector-ref sa (sub1 idx)))
        (define same-pair?
          (and (= (vector-ref rank i) (vector-ref rank prev-i))
               (= (if (< (+ i k) n) (vector-ref rank (+ i k)) -1)
                  (if (< (+ prev-i k) n) (vector-ref rank (+ prev-i k)) -1))))
        (vector-set! new-rank i
                     (if same-pair?
                         (vector-ref new-rank prev-i)
                         idx)))
      (set! rank new-rank)
      (loop (* k 2))))

  sa)

;; Parallel suffix array using prefix-doubling with thread parallelism
(define (suffix-array-parallel text workers)
  (define n (string-length text))
  (define text-vec (list->vector (string->list text)))

  ;; Initial rank based on first character
  (define rank (make-vector n 0))
  (for ([i (in-range n)])
    (vector-set! rank i (char->integer (vector-ref text-vec i))))

  ;; Create suffix array (initially just positions)
  (define sa (for/vector ([i (in-range n)]) i))

  (define (make-ranges len num-workers)
    (cond
      [(or (zero? len) (zero? num-workers)) '()]
      [else
       (define chunk (max 1 (ceiling (/ len num-workers))))
       (for/list ([start (in-range 0 len chunk)])
         (cons start (min len (+ start chunk))))]))

  (let loop ([k 1])
    (when (< k n)
      ;; compute secondary ranks (rank[i + k]) in parallel
      (define secondary (make-vector n -1))
      (define ranges (make-ranges n workers))
      (define threads1
        (for/list ([rg (in-list ranges)])
          (define start (car rg))
          (define end (cdr rg))
          (thread
           (λ ()
             (for ([i (in-range start end)])
               (vector-set! secondary i
                            (if (< (+ i k) n)
                                (vector-ref rank (+ i k))
                                -1)))))))
      (for-each thread-wait threads1)

      ;; Sort suffixes using the computed key pairs
      (define sa-list (vector->list sa))
      (define sorted-sa
        (sort sa-list
              (λ (i j)
                (define ri (vector-ref rank i))
                (define rj (vector-ref rank j))
                (if (= ri rj)
                    (< (vector-ref secondary i)
                       (vector-ref secondary j))
                    (< ri rj)))))
      (set! sa (list->vector sorted-sa))

      ;; Determine rank boundaries in parallel
      (define diff (make-vector n 0))
      (define diff-length (if (> n 0) (sub1 n) 0))
      (define diff-ranges (make-ranges diff-length workers))
      (define threads2
        (for/list ([rg (in-list diff-ranges)])
          (define start (car rg))
          (define end (cdr rg))
          (thread
           (λ ()
             (for ([offset (in-range start end)])
               (define idx (add1 offset))
               (define curr (vector-ref sa idx))
               (define prev (vector-ref sa (sub1 idx)))
               (define same-primary (= (vector-ref rank curr)
                                       (vector-ref rank prev)))
               (define same-secondary (= (vector-ref secondary curr)
                                         (vector-ref secondary prev)))
               (vector-set! diff idx (if (and same-primary same-secondary) 0 1)))))))
      (for-each thread-wait threads2)

      ;; Prefix sum (sequential) to assign new ranks
      (define new-rank (make-vector n 0))
      (when (> n 0)
        (vector-set! new-rank (vector-ref sa 0) 0))
      (define current-rank 0)
      (for ([idx (in-range 1 n)])
        (when (= 1 (vector-ref diff idx))
          (set! current-rank (add1 current-rank)))
        (vector-set! new-rank (vector-ref sa idx) current-rank))

      (set! rank new-rank)

      ;; Early exit if ranks are unique
      (unless (= current-rank (sub1 n))
        (loop (* 2 k)))))

  sa)

;; Verify suffix array correctness
(define (verify-suffix-array text sa)
  (define n (string-length text))

  ;; Check that sa is a permutation of [0, n)
  (define seen (make-vector n #f))
  (for ([i (in-vector sa)])
    (when (or (< i 0) (>= i n))
      (error 'verify-suffix-array "Invalid suffix index: ~a" i))
    (when (vector-ref seen i)
      (error 'verify-suffix-array "Duplicate suffix index: ~a" i))
    (vector-set! seen i #t))

  ;; Check that suffixes are in lexicographic order
  (for ([i (in-range (sub1 n))])
    (define si (vector-ref sa i))
    (define sj (vector-ref sa (add1 i)))
    (define suffix-i (substring text si))
    (define suffix-j (substring text sj))
    (unless (string<=? suffix-i suffix-j)
      (error 'verify-suffix-array
             "Suffixes not in order: ~a vs ~a" suffix-i suffix-j)))

  #t)

;; Generate random text
(define (generate-random-text n alphabet-size seed)
  (random-seed seed)
  (define chars (string->list "abcdefghijklmnopqrstuvwxyz"))
  (define alphabet (take chars alphabet-size))
  (list->string
   (for/list ([i (in-range n)])
     (list-ref alphabet (random alphabet-size)))))

;; Read text from file
(define (read-text-file path)
  (file->string path))

(module+ main
  (define n 100000)
  (define alphabet-size 4)
  (define text-file #f)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "suffix-array.rkt"
    #:once-each
    [("--n") arg "Text length (for random text)"
     (set! n (parse-positive-integer arg 'suffix-array))]
    [("--alphabet") arg "Alphabet size (for random text)"
     (set! alphabet-size (parse-positive-integer arg 'suffix-array))]
    [("--text") arg "Text input file"
     (set! text-file arg)]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'suffix-array))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'suffix-array))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'suffix-array))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  ;; Load or generate text
  (define text
    (if text-file
        (begin
          (printf "Loading text from ~a...\n" text-file)
          (read-text-file text-file))
        (begin
          (printf "Generating random text (n=~a, alphabet-size=~a)...\n" n alphabet-size)
          (generate-random-text n alphabet-size seed))))

  (define text-length (string-length text))
  (printf "Text length: ~a characters\n" text-length)

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n text-length)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential suffix array (prefix doubling)...\n")
    (set! seq-result
      (run-benchmark
       (λ () (suffix-array-sequential text))
       #:name 'suffix-array
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel suffix array (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (suffix-array-parallel text workers))
     #:name 'suffix-array
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (equal? seq-result result)))
                 (error 'suffix-array "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (equal? seq-result par-result)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "\nSuffix Array Statistics:\n")
  (printf "  Text length: ~a\n" text-length)
  (printf "  Array length: ~a\n" (vector-length par-result))

  (printf "\nSample suffixes (first 5 positions):\n")
  (for ([i (in-range (min 5 (vector-length par-result)))])
    (define pos (vector-ref par-result i))
    (define suffix (substring text pos (min (+ pos 20) text-length)))
    (printf "  [~a] pos=~a: ~s~a\n" i pos suffix
            (if (> (- text-length pos) 20) "..." ""))))
