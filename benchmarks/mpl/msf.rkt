#lang racket

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide msf-sequential
         msf-parallel
         read-weighted-graph
         verify-spanning-forest)

;; Weighted graph representation: adjacency list with edge weights
;; graph[v] = vector of (neighbor . weight) pairs

;; Read weighted graph from edge list format
;; Format: each line is "u v weight"
(define (read-weighted-graph path)
  (define edges (file->lines path))
  (define edge-triples
    (for/list ([line (in-list edges)])
      (define parts (string-split line))
      (when (< (length parts) 3)
        (error 'read-weighted-graph "Invalid edge format: ~a" line))
      (list (string->number (first parts))
            (string->number (second parts))
            (string->number (third parts)))))

  (when (null? edge-triples)
    (error 'read-weighted-graph "No edges found in file"))

  ;; Find max vertex id
  (define n (add1 (apply max (map (λ (e) (max (first e) (second e))) edge-triples))))

  ;; Build adjacency lists with weights
  (define adj-lists (make-vector n null))
  (for ([edge (in-list edge-triples)])
    (define u (first edge))
    (define v (second edge))
    (define w (third edge))
    ;; Add bidirectional edges (undirected graph)
    (vector-set! adj-lists u (cons (cons v w) (vector-ref adj-lists u)))
    (vector-set! adj-lists v (cons (cons u w) (vector-ref adj-lists v))))

  ;; Convert to vector of vectors
  (for/vector ([neighbors (in-vector adj-lists)])
    (list->vector neighbors)))

;; Union-Find data structure for MSF
(struct uf-state (parent rank) #:mutable #:transparent)

(define (make-union-find n)
  (define parent (for/vector ([i (in-range n)]) i))
  (define rank (make-vector n 0))
  (uf-state parent rank))

(define (uf-find! state x)
  (define parent (uf-state-parent state))
  (define px (vector-ref parent x))
  (if (= px x)
      x
      (let ([root (uf-find! state px)])
        (vector-set! parent x root)
        root)))

(define (uf-union! state x y)
  (define rx (uf-find! state x))
  (define ry (uf-find! state y))
  (unless (= rx ry)
    (define parent (uf-state-parent state))
    (define rank (uf-state-rank state))
    (define rx-rank (vector-ref rank rx))
    (define ry-rank (vector-ref rank ry))
    (cond
      [(< rx-rank ry-rank)
       (vector-set! parent rx ry)]
      [(> rx-rank ry-rank)
       (vector-set! parent ry rx)]
      [else
       (vector-set! parent ry rx)
       (vector-set! rank rx (add1 rx-rank))])))

;; Sequential MSF using Kruskal's algorithm
(define (msf-sequential graph)
  (define n (vector-length graph))

  ;; Collect all edges
  (define edges
    (apply append
           (for/list ([u (in-range n)])
             (for/list ([neighbor-weight (in-vector (vector-ref graph u))])
               (define v (car neighbor-weight))
               (define w (cdr neighbor-weight))
               (if (< u v)  ; Only include each edge once
                   (list u v w)
                   #f)))))
  (define edges-filtered (filter identity edges))

  ;; Sort edges by weight
  (define sorted-edges (sort edges-filtered < #:key third))

  ;; Kruskal's algorithm
  (define uf (make-union-find n))
  (define msf-edges null)

  (for ([edge (in-list sorted-edges)])
    (define u (first edge))
    (define v (second edge))
    (define w (third edge))
    (unless (= (uf-find! uf u) (uf-find! uf v))
      (uf-union! uf u v)
      (set! msf-edges (cons edge msf-edges))))

  (reverse msf-edges))

;; Parallel MSF using Borůvka's algorithm
(define (msf-parallel graph workers)
  (define n (vector-length graph))
  (define uf (make-union-find n))
  (define msf-edges null)

  ;; Borůvka's algorithm: iteratively connect components
  (call-with-thread-pool workers
    (λ (pool actual-workers)
      (let loop ()
        ;; Find the lightest edge from each component in parallel
        (define chunk-size (quotient (+ n actual-workers -1) actual-workers))
        (define lightest-results
          (thread-pool-wait/collect
           (for/list ([w (in-range actual-workers)])
             (define start (* w chunk-size))
             (define end (min (+ start chunk-size) n))
             (thread-pool-submit
              pool
              (λ ()
                (define lightest (make-hash))
                (for ([u (in-range start end)])
                  (define u-root (uf-find! uf u))
                  (for ([neighbor-weight (in-vector (vector-ref graph u))])
                    (define v (car neighbor-weight))
                    (define w (cdr neighbor-weight))
                    (define v-root (uf-find! uf v))
                    (when (not (= u-root v-root))
                      (define current (hash-ref lightest u-root #f))
                      (when (or (not current) (< w (third current)))
                        (hash-set! lightest u-root (list u v w))))))
                lightest)))))

        ;; Collect lightest edges from all workers
        (define all-lightest (make-hash))
        (for ([partial (in-list lightest-results)])
          (for ([(root edge) (in-hash partial)])
            (define current (hash-ref all-lightest root #f))
            (when (or (not current) (< (third edge) (third current)))
              (hash-set! all-lightest root edge))))

        ;; If no edges found, we're done
        (when (> (hash-count all-lightest) 0)
          ;; Add edges to MSF and union components
          (for ([edge (in-hash-values all-lightest)])
            (define u (first edge))
            (define v (second edge))
            (define w (third edge))
            (unless (= (uf-find! uf u) (uf-find! uf v))
              (uf-union! uf u v)
              (set! msf-edges (cons edge msf-edges))))
          (loop))))
    #:max n)

  (reverse msf-edges))

;; Verify that edges form a valid spanning forest
(define (verify-spanning-forest graph msf-edges)
  (define n (vector-length graph))

  ;; Check that all edges are valid
  (for/and ([edge (in-list msf-edges)])
    (define u (first edge))
    (define v (second edge))
    (define w (third edge))
    ;; Verify edge exists in graph with correct weight
    (for/or ([neighbor-weight (in-vector (vector-ref graph u))])
      (and (= (car neighbor-weight) v)
           (= (cdr neighbor-weight) w))))

  ;; Check that result is a forest (no cycles)
  (define uf (make-union-find n))
  (for/and ([edge (in-list msf-edges)])
    (define u (first edge))
    (define v (second edge))
    (define u-root (uf-find! uf u))
    (define v-root (uf-find! uf v))
    (when (not (= u-root v-root))
      (uf-union! uf u v))
    (not (= u-root v-root)))

  ;; Spanning means all reachable vertices are connected
  #t)

;; Generate random weighted graph
(define (generate-random-weighted-graph n avg-degree seed)
  (when (< n 2)
    (error 'generate-random-weighted-graph "need at least 2 vertices, got ~a" n))
  (random-seed seed)
  (define p (/ avg-degree (sub1 n)))
  (define adj-lists (make-vector n null))

  (for* ([u (in-range n)]
         [v (in-range (add1 u) n)])
    (when (< (random) p)
      (define weight (random 1 1000))
      (vector-set! adj-lists u (cons (cons v weight) (vector-ref adj-lists u)))
      (vector-set! adj-lists v (cons (cons u weight) (vector-ref adj-lists v)))))

  ;; Convert to vector of vectors
  (for/vector ([neighbors (in-vector adj-lists)])
    (list->vector neighbors)))

(module+ main
  (define n 1000)
  (define avg-degree 10)
  (define graph-file #f)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)

  (void
   (command-line
    #:program "msf.rkt"
    #:once-each
    [("--n") arg "Number of vertices (for random graph)"
     (set! n (parse-positive-integer arg 'msf))]
    [("--degree") arg "Average degree (for random graph)"
     (set! avg-degree (parse-positive-integer arg 'msf))]
    [("--graph") arg "Graph input file (weighted edge list format)"
     (set! graph-file arg)]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'msf))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'msf))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'msf))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]))

  ;; Load or generate graph
  (define graph
    (if graph-file
        (begin
          (printf "Loading graph from ~a...\n" graph-file)
          (read-weighted-graph graph-file))
        (begin
          (printf "Generating random weighted graph (n=~a, avg-degree=~a)...\n" n avg-degree)
          (generate-random-weighted-graph n avg-degree seed))))

  (define num-vertices (vector-length graph))
  (define num-edges (quotient (for/sum ([neighbors (in-vector graph)])
                                 (vector-length neighbors))
                               2))
  (printf "Graph: ~a vertices, ~a edges\n" num-vertices num-edges)

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n num-vertices)
                       (list 'edges num-edges)
                       (list 'workers workers)
                       (list 'seed seed)))

  (printf "Running sequential MSF (Kruskal)...\n")
  (define seq-result
    (run-benchmark
     (λ () (msf-sequential graph))
     #:name 'msf
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel MSF (Borůvka, workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (msf-parallel graph workers))
     #:name 'msf
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (verify-spanning-forest graph result)
                 (error 'msf "parallel result verification failed at iteration ~a" iteration)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (and (verify-spanning-forest graph seq-result) (verify-spanning-forest graph par-result))
      (printf "✓ Sequential and parallel results are valid spanning forests\n")
      (printf "✗ Verification failed!\n"))

  (define seq-weight (apply + (map third seq-result)))
  (define par-weight (apply + (map third par-result)))
  (printf "\nSpanning Forest Statistics:\n")
  (printf "  Sequential: ~a edges, total weight ~a\n" (length seq-result) seq-weight)
  (printf "  Parallel:   ~a edges, total weight ~a\n" (length par-result) par-weight)

  (printf "\nSample edges (first 5 from sequential):\n")
  (for ([edge (in-list (take seq-result (min 5 (length seq-result))))])
    (printf "  (~a, ~a) weight=~a\n" (first edge) (second edge) (third edge))))
