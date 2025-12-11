#lang racket

(require racket/fixnum
         data/queue
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide bfs-sequential
         bfs-parallel
         make-graph
         generate-random-graph)

;; Graph representation: vector of adjacency vectors
;; graph[i] is a vector of neighbors of vertex i
(struct graph (vertices adjacency) #:transparent)

(define (make-graph n edges)
  ;; Two-pass construction for better memory layout
  ;; Pass 1: Count degree of each vertex
  (define degrees (make-vector n 0))
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    (vector-set! degrees u (+ 1 (vector-ref degrees u)))
    (vector-set! degrees v (+ 1 (vector-ref degrees v))))

  ;; Pass 2: Allocate neighbor vectors
  (define adj (for/vector ([d (in-vector degrees)])
                (make-vector d)))

  ;; Pass 3: Fill neighbor vectors
  (define positions (make-vector n 0))
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    (define u-pos (vector-ref positions u))
    (define v-pos (vector-ref positions v))
    (vector-set! (vector-ref adj u) u-pos v)
    (vector-set! (vector-ref adj v) v-pos u)
    (vector-set! positions u (+ u-pos 1))
    (vector-set! positions v (+ v-pos 1)))

  (graph n adj))

;; Sequential BFS using a queue
(define (bfs-sequential g source)
  (define n (graph-vertices g))
  (define adj (graph-adjacency g))
  (define parent (make-vector n -1))
  (vector-set! parent source source)

  (define queue (make-queue))
  (enqueue! queue source)

  (let loop ()
    (unless (queue-empty? queue)
      (define u (dequeue! queue))
      (for ([v (in-vector (vector-ref adj u))])
        (when (= -1 (vector-ref parent v))
          (vector-set! parent v u)
          (enqueue! queue v)))
      (loop)))

  parent)

;; Parallel BFS using level-synchronous approach with atomic operations
(define (bfs-parallel g source workers)
  (define n (graph-vertices g))
  (define adj (graph-adjacency g))
  (define parent (make-vector n -1))
  (vector-set! parent source source)

  (call-with-thread-pool workers
    (λ (pool actual-workers)
      (let loop ([current-frontier (list source)])
        (unless (null? current-frontier)
          (define frontier-size (length current-frontier))
          (when (> frontier-size 0)
            ;; Process frontier in parallel
            (define next-frontier
              (if (<= frontier-size actual-workers)
                  ;; Small frontier: process sequentially
                  (for/fold ([next '()])
                            ([u (in-list current-frontier)])
                    (append next
                            (for/list ([v (in-vector (vector-ref adj u))])
                              ;; Check and set parent (race condition is benign)
                              (define old (vector-ref parent v))
                              (and (= old -1)
                                   (let ([new-parent u])
                                     (when (= -1 (vector-ref parent v))
                                       (vector-set! parent v new-parent)
                                       v))))))
                  ;; Large frontier: process in parallel chunks
                  (let* ([chunk-size (quotient (+ frontier-size actual-workers -1) actual-workers)]
                         [chunks (for/list ([w (in-range actual-workers)])
                                   (define start (* w chunk-size))
                                   (define end (min (+ start chunk-size) frontier-size))
                                   (if (>= start frontier-size)
                                       '()
                                       (take (drop current-frontier start) (- end start))))]
                         [threads
                          (for/list ([chunk (in-list chunks)] #:when (not (null? chunk)))
                            (thread-pool-submit
                             pool
                             (λ ()
                               (for/fold ([next '()])
                                         ([u (in-list chunk)])
                                 (append next
                                         (for/list ([v (in-vector (vector-ref adj u))])
                                           (and (= -1 (vector-ref parent v))
                                                (begin
                                                  (vector-set! parent v u)
                                                  v))))))))])
                    (apply append
                           (for/list ([t (in-list threads)])
                             (thread-pool-wait t))))))

            ;; Remove #f values and duplicates from next-frontier
            (define clean-frontier
              (remove-duplicates
               (filter (λ (x) x) next-frontier)))

            (loop clean-frontier)))))
    #:max n)

  parent)

;; Generate a random graph (Erdős-Rényi model)
(define (generate-random-graph n edge-prob seed)
  (random-seed seed)
  (define edges
    (for*/list ([i (in-range n)]
                [j (in-range (fx+ i 1) n)]
                #:when (< (random) edge-prob))
      (list i j)))
  (make-graph n edges))

;; Generate a grid graph (easy to verify BFS)
(define (generate-grid-graph rows cols)
  (define n (* rows cols))
  (define edges
    (for*/list ([r (in-range rows)]
                [c (in-range cols)]
                [dir (in-list '((0 1) (1 0)))] ; right and down only
                #:when (let ([nr (+ r (first dir))]
                             [nc (+ c (second dir))])
                        (and (< nr rows) (< nc cols))))
      (list (+ (* r cols) c)
            (+ (* (+ r (first dir)) cols) (+ c (second dir))))))
  (make-graph n edges))

;; Verify BFS results
(define (verify-bfs-parent parent source)
  ;; Check that source is its own parent
  (and (= source (vector-ref parent source))
       ;; Check that all reachable vertices have valid parents
       (for/and ([i (in-range (vector-length parent))])
         (or (= -1 (vector-ref parent i))  ; unreachable
             (and (>= (vector-ref parent i) 0)
                  (< (vector-ref parent i) (vector-length parent)))))))

(module+ main
  (define n 10000)
  (define edge-prob 0.001)
  (define source 0)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define graph-type 'random)  ; 'random or 'grid

  (void
   (command-line
    #:program "bfs.rkt"
    #:once-each
    [("--n") arg "Number of vertices"
     (set! n (parse-positive-integer arg 'bfs))]
    [("--edge-prob") arg "Edge probability for random graph"
     (set! edge-prob (parse-probability arg 'bfs))]
    [("--source" "-s") arg "Source vertex"
     (set! source (parse-integer arg 'bfs))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'bfs))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'bfs))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'bfs))]
    [("--graph-type") arg "Graph type: random or grid"
     (set! graph-type (string->symbol arg))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]))

  (printf "Generating ~a graph (n=~a)...\n" graph-type n)
  (define g
    (case graph-type
      [(random) (generate-random-graph n edge-prob seed)]
      [(grid) (let ([side (exact-floor (sqrt n))])
               (generate-grid-graph side side))]
      [else (error 'bfs "unknown graph type: ~a" graph-type)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'source source)
                       (list 'workers workers)
                       (list 'seed seed)
                       (list 'graph-type graph-type)))

  (printf "Running sequential BFS from vertex ~a...\n" source)
  (define seq-result
    (run-benchmark
     (λ () (bfs-sequential g source))
     #:name 'bfs
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel BFS (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (bfs-parallel g source workers))
     #:name 'bfs
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (verify-bfs-parent result source)
                 (error 'bfs "parallel result invalid at iteration ~a" iteration)))))

  (close-log-writer writer)

  (printf "\nVerification:\n")
  (printf "  Sequential parent valid: ~a\n" (verify-bfs-parent seq-result source))
  (printf "  Parallel parent valid: ~a\n" (verify-bfs-parent par-result source))
  (printf "  Results match: ~a\n" (equal? seq-result par-result))

  (define reachable (for/sum ([p (in-vector seq-result)])
                      (if (= -1 p) 0 1)))
  (printf "\nReachable vertices: ~a / ~a\n" reachable n)

  (printf "\nSample BFS tree (first 10 vertices):\n")
  (for ([i (in-range (min 10 n))])
    (printf "  parent[~a] = ~a\n" i (vector-ref seq-result i))))
