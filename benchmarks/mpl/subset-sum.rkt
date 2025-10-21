#lang racket

;; Port of subset-sum from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/subset-sum
;; Adapted for Racket parallel benchmarking
;;
;; Subset Sum Problem:
;; Given a set of integers and a target sum, find if there exists a subset
;; that sums to the target (decision version) or find such a subset.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide subset-sum-sequential
         subset-sum-parallel)

;; Generate random positive integers for subset sum
(define (generate-random-bag n max-value seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (for/vector ([i (in-range n)])
      (add1 (random max-value)))))  ; Values from 1 to max-value

;; Sequential subset sum - dynamic programming approach
;; Returns #t if a subset summing to goal exists, #f otherwise
(define (subset-sum-sequential bag goal)
  (define n (vector-length bag))
  (define dp (make-hash))

  ;; dp[i][sum] = true if subset of bag[0..i-1] sums to sum
  (define (can-sum i current-sum)
    (cond
      [(= current-sum goal) #t]
      [(or (< current-sum 0) (> current-sum goal) (>= i n)) #f]
      [else
       (define key (cons i current-sum))
       (hash-ref! dp key
                  (lambda ()
                    (or (can-sum (add1 i) current-sum)  ; Don't take current
                        (can-sum (add1 i) (+ current-sum (vector-ref bag i))))))]))  ; Take current

  (can-sum 0 0))

;; Parallel subset sum using work splitting
;; Parallelizes by exploring different branches of the search tree
(define (subset-sum-parallel bag goal workers)
  (define n (vector-length bag))

  ;; For smaller problems or deep recursion, use sequential
  (define (can-sum-sequential bag-start i current-sum)
    (cond
      [(= current-sum goal) #t]
      [(or (< current-sum 0) (> current-sum goal) (>= i n)) #f]
      [else
       (or (can-sum-sequential bag-start (add1 i) current-sum)
           (can-sum-sequential bag-start (add1 i) (+ current-sum (vector-ref bag i))))]))

  ;; Try different initial splits in parallel
  (define (try-parallel-splits)
    ;; Try taking or not taking first few elements in parallel
    (define splits (min 4 n))  ; Try up to 4 initial splits
    (define futures
      (for/list ([mask (in-range (expt 2 splits))])
        (future
         (lambda ()
           (define initial-sum
             (for/sum ([i (in-range splits)])
               (if (bitwise-bit-set? mask i)
                   (vector-ref bag i)
                   0)))
           (if (= initial-sum goal)
               #t
               (can-sum-sequential bag splits initial-sum))))))

    (for/or ([f futures])
      (touch f)))

  (if (< n 4)
      (subset-sum-sequential bag goal)
      (try-parallel-splits)))

(module+ main
  (define n 25)
  (define goal 1000)
  (define max-value 200)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "subset-sum"
   #:once-each
   [("--n") arg "Number of elements (default: 25)"
    (set! n (string->number arg))]
   [("--goal") arg "Target sum (default: 1000)"
    (set! goal (string->number arg))]
   [("--max-value") arg "Maximum element value (default: 200)"
    (set! max-value (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)])

  ;; Generate input data
  (define bag (generate-random-bag n max-value seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'goal goal)
                       (list 'max-value max-value)
                       (list 'seed seed)
                       (list 'workers workers)))

  (printf "Running sequential subset-sum(n=~a, goal=~a)...\n" n goal)
  (define seq-result
    (run-benchmark
     (λ () (subset-sum-sequential bag goal))
     #:name 'subset-sum
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel subset-sum(n=~a, goal=~a) (workers=~a)...\n" n goal workers)
  (define par-result
    (run-benchmark
     (λ () (subset-sum-parallel bag goal workers))
     #:name 'subset-sum
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (equal? seq-result result)
                 (error 'subset-sum "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (equal? seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "subset-sum found: ~a\n" seq-result))
