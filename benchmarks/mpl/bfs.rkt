#lang racket

(require racket/fixnum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide bfs-sequential
         bfs-parallel
         make-graph
         generate-random-graph)

;; ============================================================================
;; Graph representation
;; ============================================================================

;; Graph stored as vector of adjacency vectors for cache efficiency
(struct graph (vertices adjacency) #:transparent)

(define (make-graph n edges)
  ;; First count degrees
  (define degrees (make-vector n 0))
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    (vector-set! degrees u (fx+ 1 (vector-ref degrees u)))
    (vector-set! degrees v (fx+ 1 (vector-ref degrees v))))

  ;; Allocate adjacency vectors
  (define adj (for/vector ([d (in-vector degrees)]) (make-vector d 0)))
  (define pos (make-vector n 0))

  ;; Fill adjacency lists
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    ;; Add v to adj[u]
    (vector-set! (vector-ref adj u) (vector-ref pos u) v)
    (vector-set! pos u (fx+ 1 (vector-ref pos u)))
    ;; Add u to adj[v]
    (vector-set! (vector-ref adj v) (vector-ref pos v) u)
    (vector-set! pos v (fx+ 1 (vector-ref pos v))))

  (graph n adj))

;; ============================================================================
;; Sequential BFS (frontier-based, same algorithm as parallel)
;; ============================================================================

(define (bfs-sequential g source)
  (define n (graph-vertices g))
  (define adj (graph-adjacency g))
  (define parent (make-vector n -1))
  (vector-set! parent source source)

  ;; Use vector-based frontier (same approach as parallel version)
  (let loop ([frontier (vector source)])
    (define frontier-size (vector-length frontier))
    (when (> frontier-size 0)
      (define next-frontier
        (list->vector
         (for*/list ([i (in-range frontier-size)]
                     [u (in-value (vector-ref frontier i))]
                     [v (in-vector (vector-ref adj u))]
                     #:when (fx= -1 (vector-ref parent v)))
           (vector-set! parent v u)
           v)))
      (loop next-frontier)))

  parent)

;; ============================================================================
;; Parallel BFS with CAS-based frontier claiming
;; ============================================================================

;; Granularity threshold
(define GRAIN 1000)

;; Parallel BFS using CAS for claiming vertices
(define (bfs-parallel g source workers)
  (define n (graph-vertices g))
  (define adj (graph-adjacency g))

  ;; Parent array: -1 = unvisited, source = root, other = parent vertex
  ;; We use vector-cas! to atomically claim vertices
  (define parent (make-vector n -1))
  (vector-set! parent source source)

  ;; Create thread pool for true OS-level parallelism
  (define pool (make-parallel-thread-pool workers))

  ;; Double-buffered frontiers as vectors
  (define current-frontier (vector source))

  (let loop ([frontier current-frontier])
    (define frontier-size (vector-length frontier))
    (when (> frontier-size 0)
      (define next-frontier
        (if (< frontier-size GRAIN)
            ;; Small frontier: sequential processing
            (list->vector
             (for*/list ([i (in-range frontier-size)]
                         [u (in-value (vector-ref frontier i))]
                         [v (in-vector (vector-ref adj u))]
                         #:when (and (fx= -1 (vector-ref parent v))
                                     ;; CAS to claim this vertex
                                     (unsafe-vector*-cas! parent v -1 u)))
               v))
            ;; Large frontier: parallel processing with CAS
            (let* ([chunk-size (max 1 (quotient frontier-size workers))]
                   [channels
                    (for/list ([w (in-range workers)])
                      (define start (fx* w chunk-size))
                      (define end (if (fx= w (fx- workers 1))
                                      frontier-size
                                      (min (fx+ start chunk-size) frontier-size)))
                      (if (fx>= start frontier-size)
                          #f
                          (let ([ch (make-channel)])
                            (thread #:pool pool
                             (lambda ()
                               (channel-put ch
                                 (for*/list ([i (in-range start end)]
                                             [u (in-value (vector-ref frontier i))]
                                             [v (in-vector (vector-ref adj u))]
                                             ;; Try to claim with CAS - only succeeds once per vertex
                                             #:when (and (fx= -1 (vector-ref parent v))
                                                         (unsafe-vector*-cas! parent v -1 u)))
                                   v))))
                            ch)))]
                   [results (for/list ([ch channels] #:when ch) (channel-get ch))])
              (list->vector (apply append results)))))

      (loop next-frontier)))

  (parallel-thread-pool-close pool)
  parent)

;; ============================================================================
;; Graph Generation
;; ============================================================================

(define (generate-random-graph n edge-prob seed)
  (random-seed seed)
  (define edges
    (for*/list ([i (in-range n)]
                [j (in-range (fx+ i 1) n)]
                #:when (< (random) edge-prob))
      (list i j)))
  (make-graph n edges))

(define (generate-grid-graph rows cols)
  (define n (* rows cols))
  (define edges
    (for*/list ([r (in-range rows)]
                [c (in-range cols)]
                [dir (in-list '((0 1) (1 0)))]
                #:when (let ([nr (+ r (first dir))]
                             [nc (+ c (second dir))])
                        (and (< nr rows) (< nc cols))))
      (list (+ (* r cols) c)
            (+ (* (+ r (first dir)) cols) (+ c (second dir))))))
  (make-graph n edges))

;; ============================================================================
;; Verification
;; ============================================================================

(define (verify-bfs-parent parent source)
  (and (= source (vector-ref parent source))
       (for/and ([i (in-range (vector-length parent))])
         (or (= -1 (vector-ref parent i))
             (and (>= (vector-ref parent i) 0)
                  (< (vector-ref parent i) (vector-length parent)))))))

;; ============================================================================
;; Main
;; ============================================================================

(module+ main
  (define n 10000)
  (define edge-prob 0.001)
  (define source 0)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define graph-type 'random)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "bfs.rkt"
    #:once-each
    [("--n") arg "Number of vertices"
     (set! n (parse-positive-integer arg 'bfs))]
    [("--edge-prob") arg "Edge probability"
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
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

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

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential BFS from vertex ~a...\n" source)
    (set! seq-result
      (run-benchmark
       (lambda () (bfs-sequential g source))
       #:name 'bfs
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel BFS (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (lambda () (bfs-parallel g source workers))
     #:name 'bfs
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification:\n")
    (printf "  Sequential valid: ~a\n" (verify-bfs-parent seq-result source))
    (printf "  Parallel valid: ~a\n" (verify-bfs-parent par-result source))
    ;; Check same reachability
    (define seq-reachable (for/sum ([p (in-vector seq-result)]) (if (= -1 p) 0 1)))
    (define par-reachable (for/sum ([p (in-vector par-result)]) (if (= -1 p) 0 1)))
    (printf "  Same reachability: ~a (~a vs ~a)\n"
            (= seq-reachable par-reachable) seq-reachable par-reachable))

  (define reachable (for/sum ([p (in-vector par-result)]) (if (= -1 p) 0 1)))
  (printf "\nReachable vertices: ~a / ~a\n" reachable n))
