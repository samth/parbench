#lang racket

;; Direction-Optimizing BFS (matching MPL's NondetBFS algorithm)
;; Switches between top-down (sparse) and bottom-up (dense) based on frontier size

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

(struct graph (vertices edges adjacency offsets) #:transparent)

(define (make-graph n edges)
  ;; First count degrees
  (define degrees (make-vector n 0))
  (define edge-count 0)
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    (vector-set! degrees u (fx+ 1 (vector-ref degrees u)))
    (vector-set! degrees v (fx+ 1 (vector-ref degrees v)))
    (set! edge-count (fx+ edge-count 1)))

  ;; Compute offsets (CSR format)
  (define offsets (make-vector (fx+ n 1) 0))
  (for ([i (in-range n)])
    (vector-set! offsets (fx+ i 1)
                 (fx+ (vector-ref offsets i) (vector-ref degrees i))))

  ;; Allocate edge array
  (define total-edges (vector-ref offsets n))
  (define adj (make-vector total-edges 0))
  (define pos (vector-copy offsets))

  ;; Fill adjacency array
  (for ([edge (in-list edges)])
    (match-define (list u v) edge)
    (vector-set! adj (vector-ref pos u) v)
    (vector-set! pos u (fx+ 1 (vector-ref pos u)))
    (vector-set! adj (vector-ref pos v) u)
    (vector-set! pos v (fx+ 1 (vector-ref pos v))))

  (graph n edge-count adj offsets))

(define (graph-degree g v)
  (fx- (vector-ref (graph-offsets g) (fx+ v 1))
       (vector-ref (graph-offsets g) v)))

(define (graph-neighbors g v)
  (define start (vector-ref (graph-offsets g) v))
  (define end (vector-ref (graph-offsets g) (fx+ v 1)))
  (values (graph-adjacency g) start end))

;; ============================================================================
;; Sequential BFS (Same work as parallel, minus threads/channels/CAS)
;; ============================================================================

;; Sequential bottom-up: same work distribution as parallel version
(define (bottom-up-sequential g parent frontier workers)
  (define n (graph-vertices g))

  ;; Create frontier flags for O(1) membership test (same as parallel)
  (define in-frontier (make-vector n #f))
  (for ([v (in-vector frontier)])
    (vector-set! in-frontier v #t))

  ;; Process vertices in chunks like parallel version
  (define chunk-size (max 1 (quotient n workers)))
  (define results
    (for/list ([w (in-range workers)])
      (define start (fx* w chunk-size))
      (define end (if (fx= w (fx- workers 1)) n (min (fx+ start chunk-size) n)))
      (if (fx>= start n)
          '()
          (for/list ([v (in-range start end)]
                     #:when (fx= -1 (vector-ref parent v)))
            ;; Check if any neighbor is in frontier
            (define-values (adj adj-start adj-end) (graph-neighbors g v))
            (let loop ([j adj-start])
              (cond
                [(fx>= j adj-end) #f]
                [(vector-ref in-frontier (vector-ref adj j))
                 (define u (vector-ref adj j))
                 (vector-set! parent v u)
                 v]
                [else (loop (fx+ j 1))]))))))
  (list->vector (filter values (apply append results))))

;; Sequential top-down: same work distribution as parallel version
(define (top-down-sequential g parent frontier workers)
  (define frontier-size (vector-length frontier))

  ;; Compute offsets for work distribution (same as parallel)
  (define offsets (make-vector (fx+ frontier-size 1) 0))
  (for ([i (in-range frontier-size)])
    (define v (vector-ref frontier i))
    (vector-set! offsets (fx+ i 1)
                 (fx+ (vector-ref offsets i) (graph-degree g v))))
  (define total-work (vector-ref offsets frontier-size))

  (if (< total-work GRAIN)
      ;; Small work: simple sequential (same as parallel's small-work path)
      (let ([result-lists
             (for/list ([i (in-range frontier-size)])
               (define u (vector-ref frontier i))
               (define-values (adj start end) (graph-neighbors g u))
               (for/list ([idx (in-range start end)])
                 (define v (vector-ref adj idx))
                 (if (fx= -1 (vector-ref parent v))
                     (begin (vector-set! parent v u) v)
                     #f)))])
        (list->vector (filter values (apply append result-lists))))
      ;; Large work: same chunking logic as parallel
      (let* ([chunk-size (max 1 (quotient total-work workers))]
             [results
              (for/list ([w (in-range workers)])
                (define work-start (fx* w chunk-size))
                (define work-end (if (fx= w (fx- workers 1))
                                     total-work
                                     (min (fx+ work-start chunk-size) total-work)))
                (if (fx>= work-start total-work)
                    '()
                    (let loop-chunks ([work-pos work-start]
                                      [vertex-idx 0]
                                      [acc '()])
                      (if (fx>= work-pos work-end)
                          acc
                          ;; Find which frontier vertex owns this work position
                          (let find-vertex ([vi vertex-idx])
                            (cond
                              [(fx>= vi frontier-size) acc]
                              [(fx< work-pos (vector-ref offsets (fx+ vi 1)))
                               ;; This vertex owns work-pos
                               (define u (vector-ref frontier vi))
                               (define-values (adj adj-start adj-end) (graph-neighbors g u))
                               (define edge-offset (fx- work-pos (vector-ref offsets vi)))
                               (define edges-to-process
                                 (min (fx- adj-end (fx+ adj-start edge-offset))
                                      (fx- work-end work-pos)))
                               (define new-vertices
                                 (filter values
                                   (for/list ([k (in-range edges-to-process)])
                                     (define j (fx+ adj-start edge-offset k))
                                     (define v (vector-ref adj j))
                                     (if (fx= -1 (vector-ref parent v))
                                         (begin (vector-set! parent v u) v)
                                         #f))))
                               (loop-chunks (fx+ work-pos edges-to-process)
                                            vi
                                            (append acc new-vertices))]
                              [else (find-vertex (fx+ vi 1))]))))))])
        (list->vector (apply append results)))))

(define (bfs-sequential g source workers)
  (define n (graph-vertices g))
  (define parent (make-vector n -1))
  (vector-set! parent source source)

  (let loop ([frontier (vector source)])
    (define frontier-size (vector-length frontier))
    (when (> frontier-size 0)
      (define next-frontier
        (if (should-process-dense? g frontier)
            (bottom-up-sequential g parent frontier workers)
            (top-down-sequential g parent frontier workers)))
      (loop next-frontier)))
  parent)

;; ============================================================================
;; Direction-Optimizing Parallel BFS
;; ============================================================================

(define GRAIN 10000)

;; Sum of out-degrees for frontier
(define (sum-of-degrees g frontier)
  (for/sum ([v (in-vector frontier)])
    (graph-degree g v)))

;; Decide whether to use bottom-up (dense) mode
(define (should-process-dense? g frontier)
  (define dense-threshold (quotient (* 2 (graph-edges g)) 20))
  (define n (vector-length frontier))
  (define m (sum-of-degrees g frontier))
  (> (+ n m) dense-threshold))

;; Bottom-up: for each unvisited vertex, check if any neighbor is in frontier
(define (bottom-up g parent frontier pool workers)
  (define n (graph-vertices g))

  ;; Create frontier flags for O(1) membership test
  (define in-frontier (make-vector n #f))
  (for ([v (in-vector frontier)])
    (vector-set! in-frontier v #t))

  ;; Process all unvisited vertices in parallel
  (define chunk-size (max 1 (quotient n workers)))
  (define channels
    (for/list ([w (in-range workers)])
      (define start (fx* w chunk-size))
      (define end (if (fx= w (fx- workers 1)) n (min (fx+ start chunk-size) n)))
      (if (fx>= start n)
          #f
          (let ([ch (make-channel)])
            (thread #:pool pool
             (lambda ()
               (channel-put ch
                 (for/list ([v (in-range start end)]
                            #:when (fx= -1 (vector-ref parent v)))
                   ;; Check if any neighbor is in frontier
                   (define-values (adj adj-start adj-end) (graph-neighbors g v))
                   (let loop ([j adj-start])
                     (cond
                       [(fx>= j adj-end) #f]
                       [(vector-ref in-frontier (vector-ref adj j))
                        (define u (vector-ref adj j))
                        (when (unsafe-vector*-cas! parent v -1 u)
                          v)]
                       [else (loop (fx+ j 1))]))))))
            ch))))

  (define results (for/list ([ch channels] #:when ch) (channel-get ch)))
  (list->vector (filter values (apply append results))))

;; Top-down: expand from frontier using CAS
(define (top-down g parent frontier pool workers)
  (define frontier-size (vector-length frontier))

  ;; Compute offsets for work distribution
  (define offsets (make-vector (fx+ frontier-size 1) 0))
  (for ([i (in-range frontier-size)])
    (define v (vector-ref frontier i))
    (vector-set! offsets (fx+ i 1)
                 (fx+ (vector-ref offsets i) (graph-degree g v))))
  (define total-work (vector-ref offsets frontier-size))

  (if (< total-work GRAIN)
      ;; Small work: sequential
      (let ([result-lists
             (for/list ([i (in-range frontier-size)])
               (define u (vector-ref frontier i))
               (define-values (adj start end) (graph-neighbors g u))
               (for/list ([idx (in-range start end)])
                 (define v (vector-ref adj idx))
                 (if (and (fx= -1 (vector-ref parent v))
                          (unsafe-vector*-cas! parent v -1 u))
                     v
                     #f)))])
        (list->vector (filter values (apply append result-lists))))
      ;; Large work: parallel with work-balanced chunks
      (let* ([chunk-size (max 1 (quotient total-work workers))]
             [channels
              (for/list ([w (in-range workers)])
                (define work-start (fx* w chunk-size))
                (define work-end (if (fx= w (fx- workers 1))
                                     total-work
                                     (min (fx+ work-start chunk-size) total-work)))
                (if (fx>= work-start total-work)
                    #f
                    (let ([ch (make-channel)])
                      (thread #:pool pool
                       (lambda ()
                         (channel-put ch
                           (let loop-chunks ([work-pos work-start]
                                             [vertex-idx 0]
                                             [acc '()])
                             (if (fx>= work-pos work-end)
                                 acc
                                 ;; Find which frontier vertex owns this work position
                                 (let find-vertex ([vi vertex-idx])
                                   (cond
                                     [(fx>= vi frontier-size) acc]
                                     [(fx< work-pos (vector-ref offsets (fx+ vi 1)))
                                      ;; This vertex owns work-pos
                                      (define u (vector-ref frontier vi))
                                      (define-values (adj adj-start adj-end) (graph-neighbors g u))
                                      (define edge-offset (fx- work-pos (vector-ref offsets vi)))
                                      (define edges-to-process
                                        (min (fx- adj-end (fx+ adj-start edge-offset))
                                             (fx- work-end work-pos)))
                                      (define new-vertices
                                        (filter values
                                          (for/list ([k (in-range edges-to-process)])
                                            (define j (fx+ adj-start edge-offset k))
                                            (define v (vector-ref adj j))
                                            (if (and (fx= -1 (vector-ref parent v))
                                                     (unsafe-vector*-cas! parent v -1 u))
                                                v
                                                #f))))
                                      (loop-chunks (fx+ work-pos edges-to-process)
                                                   vi
                                                   (append acc new-vertices))]
                                     [else (find-vertex (fx+ vi 1))])))))))
                      ch)))]
             [results (for/list ([ch channels] #:when ch) (channel-get ch))])
        (list->vector (apply append results)))))

;; Main parallel BFS with direction optimization
(define (bfs-parallel g source workers #:diropt [diropt #t])
  (define n (graph-vertices g))
  (define parent (make-vector n -1))
  (vector-set! parent source source)

  (define pool (make-parallel-thread-pool workers))

  (let loop ([frontier (vector source)])
    (define frontier-size (vector-length frontier))
    (when (> frontier-size 0)
      (define next-frontier
        (if (and diropt (should-process-dense? g frontier))
            (bottom-up g parent frontier pool workers)
            (top-down g parent frontier pool workers)))
      (loop (if (vector? next-frontier)
                next-frontier
                (list->vector (apply append (vector->list next-frontier)))))))

  (parallel-thread-pool-close pool)
  parent)

;; ============================================================================
;; Graph Generation
;; ============================================================================

;; RMAT graph generator - O(m log n) instead of O(n²)
;; Uses recursive matrix model with parameters (a, b, c, d)
;; Default params approximate social network structure
;;
;; NOTE: How MPL does graph generation:
;; - MPL does NOT generate graphs at runtime
;; - Uses pbbsbench's C++ rMatGraph tool: ./rMatGraph -s seed -o -j num_edges output_file
;; - The C++ tool uses parlay::tabulate(m, ...) to generate all m edges IN PARALLEL
;; - Each edge generated independently in O(log n) via recursive quadrant selection
;; - No deduplication during generation - duplicates/self-loops filtered at load time
;; - Graphs are pre-generated and stored as binary files for fast loading
;;
;; Our implementation: sequential generation, skip deduplication for speed
(define (generate-rmat-graph n m seed
                              #:a [a 0.57] #:b [b 0.19] #:c [c 0.19] #:d [d 0.05])
  (random-seed seed)
  ;; Find k such that 2^k >= n
  (define k (let loop ([k 1]) (if (>= (expt 2 k) n) k (loop (+ k 1)))))
  (define n-rounded (expt 2 k))

  ;; Cumulative probabilities for quadrant selection
  (define cum-a a)
  (define cum-ab (+ a b))
  (define cum-abc (+ a b c))

  ;; Generate one edge using RMAT recursive descent - O(log n) per edge
  (define (generate-edge)
    (let loop ([depth k] [row 0] [col 0] [size n-rounded])
      (if (= depth 0)
          (cons (modulo row n) (modulo col n))  ; wrap to actual n
          (let* ([half (quotient size 2)]
                 [r (random)]
                 [quadrant (cond [(< r cum-a) 0]      ; top-left
                                 [(< r cum-ab) 1]     ; top-right
                                 [(< r cum-abc) 2]    ; bottom-left
                                 [else 3])])          ; bottom-right
            (loop (- depth 1)
                  (+ row (if (>= quadrant 2) half 0))
                  (+ col (if (odd? quadrant) half 0))
                  half)))))

  ;; Generate m edges - no deduplication (like pbbsbench)
  ;; Filter self-loops only
  (define edges
    (for/list ([_ (in-range m)])
      (let loop ()
        (define edge (generate-edge))
        (if (= (car edge) (cdr edge))
            (loop)  ; retry self-loops
            (list (car edge) (cdr edge))))))

  (make-graph n edges))

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
  (define edge-count #f)  ; for RMAT
  (define edge-prob 0.001)
  (define source 0)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define graph-type 'rmat)  ; default to rmat
  (define skip-sequential #f)

  (void
   (command-line
    #:program "bfs.rkt"
    #:once-each
    [("--n") arg "Number of vertices"
     (set! n (parse-positive-integer arg 'bfs))]
    [("--edges" "-m") arg "Number of edges (for rmat)"
     (set! edge-count (parse-positive-integer arg 'bfs))]
    [("--edge-prob") arg "Edge probability (for random)"
     (set! edge-prob (parse-probability arg 'bfs))]
    [("--source" "-s") arg "Source vertex"
     (set! source (parse-integer arg 'bfs))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'bfs))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'bfs))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'bfs))]
    [("--graph-type") arg "Graph type: rmat, random, or grid"
     (set! graph-type (string->symbol arg))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  ;; Default edge count for RMAT: 10 * n
  (unless edge-count
    (set! edge-count (* 10 n)))

  (printf "Generating ~a graph (n=~a)...\n" graph-type n)
  (define g
    (case graph-type
      [(rmat) (generate-rmat-graph n edge-count seed)]
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
       (lambda () (bfs-sequential g source workers))
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
    (define seq-reachable (for/sum ([p (in-vector seq-result)]) (if (= -1 p) 0 1)))
    (define par-reachable (for/sum ([p (in-vector par-result)]) (if (= -1 p) 0 1)))
    (printf "  Same reachability: ~a (~a vs ~a)\n"
            (= seq-reachable par-reachable) seq-reachable par-reachable))

  (define reachable (for/sum ([p (in-vector par-result)]) (if (= -1 p) 0 1)))
  (printf "\nReachable vertices: ~a / ~a\n" reachable n))
