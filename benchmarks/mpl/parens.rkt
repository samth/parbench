#lang racket

;; Port of parens from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/parens
;; Adapted for Racket parallel benchmarking
;;
;; Parentheses Matching Problem:
;; Check if a sequence of parentheses is balanced using parallel reduction.
;; Maps each paren to (left_count, right_count) and reduces with a combine function.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide parens-sequential
         parens-parallel
         generate-parens)

;; Generate parentheses sequence
;; Even indices = left paren, odd indices = right paren
(define (generate-parens n)
  (for/vector ([i (in-range n)])
    (if (even? i) #\( #\))))

;; Combine function for parentheses matching
;; Each element is represented as (unmatched-left, unmatched-right)
;; Combines two such pairs
(define (combine-paren-counts p1 p2)
  (match-define (cons left1 right1) p1)
  (match-define (cons left2 right2) p2)
  (define matched (min left1 right2))
  (cons (+ (- left1 matched) left2)
        (+ right1 (- right2 matched))))

;; Sequential parentheses matching
(define (parens-sequential data)
  (define n (vector-length data))
  (define result
    (for/fold ([counts (cons 0 0)])
              ([i (in-range n)])
      (define char (vector-ref data i))
      (define elem (if (char=? char #\()
                       (cons 1 0)  ; left paren
                       (cons 0 1))) ; right paren
      (combine-paren-counts counts elem)))
  ;; Balanced if both counts are 0
  (and (= (car result) 0)
       (= (cdr result) 0)))

;; Parallel parentheses matching using reduction
(define (parens-parallel data workers)
  (define n (vector-length data))
  (define (process-range start end)
    (for/fold ([counts (cons 0 0)])
              ([i (in-range start end)])
      (define char (vector-ref data i))
      (define elem (if (char=? char #\()
                       (cons 1 0)
                       (cons 0 1)))
      (combine-paren-counts counts elem)))

  (define partial-results
    (if (<= workers 1)
        (list (process-range 0 n))
        (call-with-thread-pool workers
          (λ (pool actual-workers)
            (define effective-workers (max 1 (min actual-workers n)))
            (define chunk-size (max 1 (ceiling (/ n effective-workers))))
            (define tasks
              (for/list ([start (in-range 0 n chunk-size)])
                (define end (min n (+ start chunk-size)))
                (thread-pool-submit pool (λ () (process-range start end)))))
            (thread-pool-wait/collect tasks))
          #:max (ceiling (/ n (max 1 (min workers n)))))))

  (define final-result
    (for/fold ([acc (cons 0 0)])
              ([partial (in-list partial-results)])
      (combine-paren-counts acc partial)))

  ;; Balanced if both counts are 0
  (and (= (car final-result) 0)
       (= (cdr final-result) 0)))

(module+ main
  (define n 10000000)
  (define workers 1)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "parens"
   #:once-each
   [("--n") arg "Number of parentheses (default: 10000000)"
    (set! n (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)])

  ;; Generate input data
  (define data (generate-parens n))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'workers workers)))

  (printf "Running sequential parens(n=~a)...\n" n)
  (define seq-result
    (run-benchmark
     (λ () (parens-sequential data))
     #:name 'parens
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel parens(n=~a) (workers=~a)...\n" n workers)
  (define par-result
    (run-benchmark
     (λ () (parens-parallel data workers))
     #:name 'parens
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (equal? seq-result result)
                 (error 'parens "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (equal? seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "parens balanced: ~a\n" seq-result))
