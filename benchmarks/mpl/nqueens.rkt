#lang racket

;; Port of nqueens from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/nqueens
;; Adapted for Racket parallel benchmarking
;;
;; N-Queens Problem: Count all solutions for placing N queens on an NxN board
;; such that no two queens threaten each other.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide nqueens-sequential
         nqueens-parallel)

;; Check if placing a queen at (row, col) is threatened by any existing queen
;; queens is a list where first element is most recent placement
(define (threatened? queens row col)
  (for/or ([(other-col idx) (in-indexed (in-list queens))])
    (define other-row (- row idx 1))  ; Previous rows
    (or (= col other-col)                           ; same column
        (= (abs (- row other-row))                  ; same diagonal
           (abs (- col other-col))))))

;; Sequential N-Queens solver - returns count of solutions
(define (nqueens-sequential n)
  (define (search row queens)
    (cond
      [(= row n) 1]  ; Found a complete solution
      [else
       (for/sum ([col (in-range n)])
         (if (threatened? queens row col)
             0
             (search (add1 row) (cons col queens))))]))
  (search 0 '()))

;; Parallel N-Queens solver
;; Parallelizes at the top level (first row placements)
(define (nqueens-parallel n workers)
  (define (search row queens)
    (cond
      [(= row n) 1]  ; Found a complete solution
      [else
       (for/sum ([col (in-range n)])
         (if (threatened? queens row col)
             0
             (search (add1 row) (cons col queens))))]))

  ;; Parallelize over first row placements
  (if (<= workers 1)
      (nqueens-sequential n)
      (call-with-thread-pool workers
        (λ (pool actual-workers)
          (define tasks
            (for/list ([col (in-range n)])
              (thread-pool-submit pool (λ () (search 1 (list col))))))
          (for/sum ([task (in-list tasks)])
            (thread-pool-wait task)))
        #:max n)))

(module+ main
  (define n 12)
  (define workers 1)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "nqueens"
   #:once-each
   [("--n") arg "Board size (default: 12)"
    (set! n (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)])

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'workers workers)))

  (printf "Running sequential nqueens(~a)...\n" n)
  (define seq-result
    (run-benchmark
     (λ () (nqueens-sequential n))
     #:name 'nqueens
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel nqueens(~a) (workers=~a)...\n" n workers)
  (define par-result
    (run-benchmark
     (λ () (nqueens-parallel n workers))
     #:name 'nqueens
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (= seq-result result)
                 (error 'nqueens "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (= seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "nqueens(~a) = ~a solutions\n" n seq-result))
