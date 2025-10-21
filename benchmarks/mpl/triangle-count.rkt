#lang racket

;; Port of triangle-count from MPL parallel-ml-bench
;; Original: https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/triangle-count
;; Adapted for Racket parallel benchmarking
;;
;; Triangle Counting: Count the number of triangles in an undirected graph.
;; A triangle is a set of three vertices that are all connected to each other.
;; Uses adjacency list representation and intersection-based counting.

(require "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/run.rkt"
         "../common/parallel.rkt")

(provide triangle-count-sequential
         triangle-count-parallel
         generate-random-graph)

;; Generate random graph (adjacency list)
(define (generate-random-graph n num-edges seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed)
    (define adj (make-vector n '()))
    (for ([i (in-range num-edges)])
      (define u (random n))
      (define v (random n))
      (when (< u v)  ; Only add edge once (undirected)
        (vector-set! adj u (cons v (vector-ref adj u)))
        (vector-set! adj v (cons u (vector-ref adj v)))))
    ;; Sort adjacency lists for faster intersection
    (for ([i (in-range n)])
      (vector-set! adj i (sort (vector-ref adj i) <)))
    adj))

;; Count triangles using the node-iterator algorithm
;; For each edge (u,v), count common neighbors
(define (triangle-count-sequential graph)
  (define n (vector-length graph))
  (define count 0)

  ;; For each vertex u
  (for ([u (in-range n)])
    (define neighbors-u (vector-ref graph u))
    ;; For each neighbor v of u (where v > u to avoid double counting)
    (for ([v (in-list neighbors-u)]
          #:when (> v u))
      ;; Count common neighbors of u and v
      (define neighbors-v (vector-ref graph v))
      ;; Intersect the two sorted lists
      (let loop ([list-u neighbors-u]
                 [list-v neighbors-v])
        (cond
          [(or (null? list-u) (null? list-v)) (void)]
          [else
           (define elem-u (car list-u))
           (define elem-v (car list-v))
           (cond
             [(= elem-u elem-v)
              ;; Common neighbor found - triangle!
              ;; Only count if w > v to avoid triple counting
              (when (> elem-u v)
                (set! count (add1 count)))
              (loop (cdr list-u) (cdr list-v))]
             [(< elem-u elem-v)
              (loop (cdr list-u) list-v)]
             [else
              (loop list-u (cdr list-v))])]))))

  count)

;; Parallel triangle counting
(define (triangle-count-parallel graph workers)
  (define n (vector-length graph))

  (define (count-range start end)
    (define local-count 0)
    (for ([u (in-range start end)])
      (define neighbors-u (vector-ref graph u))
      (for ([v (in-list neighbors-u)]
            #:when (> v u))
        (define neighbors-v (vector-ref graph v))
        (let loop ([list-u neighbors-u]
                   [list-v neighbors-v])
          (cond
            [(or (null? list-u) (null? list-v)) (void)]
            [else
             (define elem-u (car list-u))
             (define elem-v (car list-v))
             (cond
               [(= elem-u elem-v)
                (when (> elem-u v)
                  (set! local-count (add1 local-count)))
                (loop (cdr list-u) (cdr list-v))]
               [(< elem-u elem-v)
                (loop (cdr list-u) list-v)]
               [else
                (loop list-u (cdr list-v))])]))))
    local-count)

  (if (<= workers 1)
      (triangle-count-sequential graph)
      (call-with-thread-pool workers
        (λ (pool actual-workers)
          (define chunk-size (max 1 (ceiling (/ n (max 1 (min actual-workers n))))))
          (define tasks
            (for/list ([start (in-range 0 n chunk-size)])
              (define end (min n (+ start chunk-size)))
              (thread-pool-submit pool (λ () (count-range start end)))))
          (apply + (thread-pool-wait/collect tasks)))
        #:max n)))

(module+ main
  (define n 1000)
  (define num-edges 5000)
  (define seed 42)
  (define workers 1)
  (define repeat 1)
  (define log-path "")

  (command-line
   #:program "triangle-count"
   #:once-each
   [("--n") arg "Number of vertices (default: 1000)"
    (set! n (string->number arg))]
   [("--edges") arg "Number of edges (default: 5000)"
    (set! num-edges (string->number arg))]
   [("--seed") arg "Random seed (default: 42)"
    (set! seed (string->number arg))]
   [("--workers") arg "Number of workers (default: 1)"
    (set! workers (string->number arg))]
   [("--repeat") arg "Number of repetitions (default: 1)"
    (set! repeat (string->number arg))]
   [("--log") arg "Log file path"
    (set! log-path arg)])

  ;; Generate input graph
  (printf "Generating random graph with ~a vertices and ~a edges...\n" n num-edges)
  (define graph (generate-random-graph n num-edges seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'num-edges num-edges)
                       (list 'seed seed)
                       (list 'workers workers)))

  (printf "Running sequential triangle-count(n=~a, edges=~a)...\n" n num-edges)
  (define seq-result
    (run-benchmark
     (λ () (triangle-count-sequential graph))
     #:name 'triangle-count
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel triangle-count(n=~a, edges=~a) (workers=~a)...\n" n num-edges workers)
  (define par-result
    (run-benchmark
     (λ () (triangle-count-parallel graph workers))
     #:name 'triangle-count
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (= seq-result result)
                 (error 'triangle-count "parallel result mismatch at iteration ~a: expected ~a, got ~a"
                        iteration seq-result result)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (= seq-result par-result)
      (printf "✓ Sequential and parallel results match\n")
      (printf "✗ Results differ!\n"))

  (printf "Found ~a triangles\n" seq-result))
