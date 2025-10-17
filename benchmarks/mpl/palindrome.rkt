#lang racket

;; Port of palindrome from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/palindrome
;; Adapted for Racket parallel benchmarking
;;
;; Palindrome checking - simple string algorithm for testing parallel comparison

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

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
      (call-with-thread-pool workers
        (λ (pool actual-workers)
          (define num-chunks (quotient (+ half chunk-size -1) chunk-size))
          (define results
            (thread-pool-wait/collect
             (for/list ([c (in-range num-chunks)])
               (define start (* c chunk-size))
               (define end (min (+ start chunk-size) half))
               (thread-pool-submit
                pool
                (λ ()
                  (for/and ([i (in-range start end)])
                    (char=? (string-ref str i)
                            (string-ref str (- n i 1)))))))))

          ;; All chunks must return #t
          (andmap values results))
        #:max half)))

;; Generate test string - palindrome or not
(define (generate-test-string n palindrome? seed)
  (random-seed seed)
  (define chars (if palindrome?
                    ;; Generate palindrome
                    (let* ([half (quotient n 2)]
                           [first-half (for/list ([i (in-range half)])
                                         (integer->char (+ 97 (random 26))))]
                           [middle (if (odd? n)
                                       (list (integer->char (+ 97 (random 26))))
                                       '())]
                           [second-half (reverse first-half)])
                      (append first-half middle second-half))
                    ;; Generate random string (likely not palindrome)
                    (for/list ([i (in-range n)])
                      (integer->char (+ 97 (random 26))))))
  (list->string chars))

(module+ main
  (define n 10000000)
  (define palindrome? #t)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define chunk-size 10000)
  (define seed 42)

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
     (set! log-path arg)]))

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

  (printf "Running sequential palindrome check...\n")
  (define seq-result
    (run-benchmark
     (λ () (palindrome-sequential str))
     #:name 'palindrome
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

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
               (unless (equal? seq-result result)
                 (error 'palindrome "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (equal? seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "Result: ~a (expected: ~a)\n"
          (if seq-result "palindrome" "not palindrome")
          (if palindrome? "palindrome" "not palindrome"))

  (when (<= n 100)
    (printf "String: ~a\n" str)))
