#lang racket

;; Port of palindrome from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/palindrome
;; Adapted for Racket parallel benchmarking
;;
;; Palindrome checking - simple string algorithm for testing parallel comparison

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide palindrome-sequential
         palindrome-parallel
         generate-test-string)

;; Sequential palindrome check
(define (palindrome-sequential str)
  (define n (string-length str))
  (define half (quotient n 2))
  (for/and ([i (in-range half)])
    (char=? (string-ref str i)
            (string-ref str (- n i 1)))))

;; Parallel palindrome check - compare chunks in parallel
(define (palindrome-parallel str workers [chunk-size 10000])
  (define n (string-length str))
  (define half (quotient n 2))

  (if (< half chunk-size)
      ;; Too small for parallelism
      (palindrome-sequential str)
      ;; Check chunks in parallel
      (let* ([pool (make-parallel-thread-pool workers)]
             [num-chunks (quotient (+ half chunk-size -1) chunk-size)]
             [channels
              (for/list ([c (in-range num-chunks)])
                (define start (* c chunk-size))
                (define end (min (+ start chunk-size) half))
                (define ch (make-channel))
                (thread #:pool pool
                 (λ ()
                   (channel-put ch
                     (for/and ([i (in-range start end)])
                       (char=? (string-ref str i)
                               (string-ref str (- n i 1)))))))
                ch)]
             [results (map channel-get channels)])
        (parallel-thread-pool-close pool)
        ;; All chunks must return #t
        (andmap values results))))

;; Generate test string - palindrome or not (using vector for efficiency)
(define (generate-test-string n palindrome? seed)
  (random-seed seed)
  (define result (make-string n))
  (define half (quotient n 2))
  (if palindrome?
      ;; Generate palindrome
      (begin
        (for ([i (in-range half)])
          (define c (integer->char (+ 97 (random 26))))
          (string-set! result i c)
          (string-set! result (- n i 1) c))
        (when (odd? n)
          (string-set! result half (integer->char (+ 97 (random 26))))))
      ;; Generate random string (likely not palindrome)
      (for ([i (in-range n)])
        (string-set! result i (integer->char (+ 97 (random 26))))))
  result)

(module+ main
  (define n 10000000)
  (define palindrome? #t)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define chunk-size 10000)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "palindrome.rkt"
    #:once-each
    [("--n") arg "String length"
     (set! n (parse-positive-integer arg 'palindrome))]
    [("--palindrome") "Generate palindrome (default: true)"
     (set! palindrome? #t)]
    [("--no-palindrome") "Generate non-palindrome"
     (set! palindrome? #f)]
    [("--chunk-size") arg "Chunk size for parallel computation"
     (set! chunk-size (parse-positive-integer arg 'palindrome))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'palindrome))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'palindrome))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'palindrome))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (printf "Generating ~a string (n=~a)...\n"
          (if palindrome? "palindrome" "non-palindrome") n)
  (define str (generate-test-string n palindrome? seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'palindrome palindrome?)
                       (list 'chunk-size chunk-size)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential palindrome check...\n")
    (set! seq-result
      (run-benchmark
       (λ () (palindrome-sequential str))
       #:name 'palindrome
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel palindrome check (workers=~a, chunk-size=~a)...\n"
          workers chunk-size)
  (define par-result
    (run-benchmark
     (λ () (palindrome-parallel str workers chunk-size))
     #:name 'palindrome
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (equal? seq-result result)))
                 (error 'palindrome "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (equal? seq-result par-result)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "Result: ~a (expected: ~a)\n"
          (if par-result "palindrome" "not palindrome")
          (if palindrome? "palindrome" "not palindrome"))

  (when (<= n 100)
    (printf "String: ~a\n" str)))
