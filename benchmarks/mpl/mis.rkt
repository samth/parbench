#lang racket

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide mis-sequential
         mis-parallel
         read-graph-edgelist
         verify-mis)

;; Graph representation: adjacency list as vector of vectors
;; graph[v] = vector of neighbors of vertex v

;; Read graph from edge list format
;; Format: each line is "u v" representing an edge
(define (read-graph-edgelist path)
  (define edges (file->lines path))
  (define edge-pairs
    (for/list ([line (in-list edges)])
      (define parts (string-split line))
      (when (< (length parts) 2)
        (error 'read-graph-edgelist "Invalid edge format: ~a" line))
      (list (string->number (first parts))
            (string->number (second parts)))))

  (when (null? edge-pairs)
    (error 'read-graph-edgelist "No edges found in file"))

  ;; Find max vertex id
  (define n (add1 (apply max (flatten edge-pairs))))

  ;; Build adjacency lists
  (define adj-lists (make-vector n null))
  (for ([edge (in-list edge-pairs)])
    (define u (first edge))
    (define v (second edge))
    ;; Add bidirectional edges (undirected graph)
    (vector-set! adj-lists u (cons v (vector-ref adj-lists u)))
    (vector-set! adj-lists v (cons u (vector-ref adj-lists v))))

  ;; Convert to vector of vectors
  (for/vector ([neighbors (in-vector adj-lists)])
    (list->vector neighbors)))

;; Sequential MIS using Luby's randomized algorithm (same as parallel, but sequential)
;; This ensures fair comparison between sequential and parallel
(define (mis-sequential graph [seed 42])
  (define n (vector-length graph))
  (define in-mis (make-vector n #f))
  (define active (make-vector n #t))
  (define active-count n)

  (random-seed seed)

  ;; Iterate until all vertices are processed
  (let loop ([round 0])
    (when (> active-count 0)
      ;; Assign random priorities to active vertices
      (define priorities (make-vector n 0))
      (for ([v (in-range n)]
            #:when (vector-ref active v))
        (vector-set! priorities v (random 1000000000)))

      ;; Find local maxima (sequentially)
      (define local-maxima (make-vector n #f))
      (for ([v (in-range n)]
            #:when (vector-ref active v))
        (define v-priority (vector-ref priorities v))
        (define is-max
          (for/and ([u (in-vector (vector-ref graph v))])
            (or (not (vector-ref active u))
                (> v-priority (vector-ref priorities u)))))
        (when is-max
          (vector-set! local-maxima v #t)))

      ;; Add local maxima to MIS and remove them and their neighbors
      (define new-removed 0)
      (for ([v (in-range n)]
            #:when (vector-ref local-maxima v))
        (vector-set! in-mis v #t)
        (vector-set! active v #f)
        (set! new-removed (add1 new-removed))
        ;; Remove neighbors
        (for ([u (in-vector (vector-ref graph v))])
          (when (vector-ref active u)
            (vector-set! active u #f)
            (set! new-removed (add1 new-removed)))))

      (set! active-count (- active-count new-removed))
      (loop (add1 round))))

  ;; Return list of vertices in MIS
  (for/list ([v (in-range n)]
             #:when (vector-ref in-mis v))
    v))

;; Parallel MIS using Luby's randomized algorithm
;; Each vertex picks a random value; if it's a local maximum among neighbors,
;; it joins the MIS. Repeat until all vertices are either in MIS or removed.
(define (mis-parallel graph workers seed)
  (define n (vector-length graph))
  (define in-mis (make-vector n #f))
  (define active (make-vector n #t))
  (define active-count n)

  (random-seed seed)

  ;; Create thread pool for true OS-level parallelism
  (define pool (make-parallel-thread-pool workers))

  ;; Iterate until all vertices are processed
  (let loop ([round 0])
    (when (> active-count 0)
      ;; Assign random priorities to active vertices
      (define priorities (make-vector n 0))
      (for ([v (in-range n)]
            #:when (vector-ref active v))
        (vector-set! priorities v (random 1000000000)))

      ;; Find local maxima in parallel - return lists instead of n-sized vectors
      (define chunk-size (quotient (+ n workers -1) workers))
      (define channels
        (for/list ([w (in-range workers)])
          (define start (* w chunk-size))
          (define end (min (+ start chunk-size) n))
          (and (< start end)
               (let ([ch (make-channel)])
                 (thread #:pool pool
                  (λ ()
                    (channel-put ch
                      (for/list ([v (in-range start end)]
                                 #:when (vector-ref active v)
                                 #:when (let ([v-priority (vector-ref priorities v)])
                                          (for/and ([u (in-vector (vector-ref graph v))])
                                            (or (not (vector-ref active u))
                                                (> v-priority (vector-ref priorities u))))))
                        v))))
                 ch))))

      ;; Collect results - just flatten the lists
      (define local-maxima-list
        (apply append (for/list ([ch (in-list channels)] #:when ch) (channel-get ch))))

      ;; Add local maxima to MIS and remove them and their neighbors
      (define new-removed 0)
      (for ([v (in-list local-maxima-list)])
        (vector-set! in-mis v #t)
        (vector-set! active v #f)
        (set! new-removed (add1 new-removed))
        ;; Remove neighbors
        (for ([u (in-vector (vector-ref graph v))])
          (when (vector-ref active u)
            (vector-set! active u #f)
            (set! new-removed (add1 new-removed)))))

      (set! active-count (- active-count new-removed))
      (loop (add1 round))))

  (parallel-thread-pool-close pool)

  ;; Return list of vertices in MIS
  (for/list ([v (in-range n)]
             #:when (vector-ref in-mis v))
    v))

;; Verify that a set is a valid maximal independent set
(define (verify-mis graph mis-vertices)
  (define n (vector-length graph))
  (define in-mis (make-vector n #f))

  ;; Mark MIS vertices
  (for ([v (in-list mis-vertices)])
    (vector-set! in-mis v #t))

  ;; Check independence: no two adjacent vertices in MIS
  (define independent?
    (for/and ([v (in-list mis-vertices)])
      (for/and ([u (in-vector (vector-ref graph v))])
        (not (vector-ref in-mis u)))))

  ;; Check maximality: every vertex not in MIS has a neighbor in MIS
  (define maximal?
    (for/and ([v (in-range n)]
              #:unless (vector-ref in-mis v))
      (for/or ([u (in-vector (vector-ref graph v))])
        (vector-ref in-mis u))))

  (and independent? maximal?))

;; Generate random graph - O(n * avg-degree) instead of O(n^2)
(define (generate-random-graph n avg-degree seed)
  (when (< n 2)
    (error 'generate-random-graph "need at least 2 vertices, got ~a" n))
  (random-seed seed)
  (define adj-lists (make-vector n null))
  (define edge-set (make-hash))  ; Track edges to avoid duplicates

  ;; For each vertex, add approximately avg-degree edges
  (for ([u (in-range n)])
    (for ([_ (in-range avg-degree)])
      (define v (random n))
      (when (and (not (= u v))
                 (not (hash-ref edge-set (cons (min u v) (max u v)) #f)))
        (hash-set! edge-set (cons (min u v) (max u v)) #t)
        (vector-set! adj-lists u (cons v (vector-ref adj-lists u)))
        (vector-set! adj-lists v (cons u (vector-ref adj-lists v))))))

  ;; Convert to vector of vectors
  (for/vector ([neighbors (in-vector adj-lists)])
    (list->vector neighbors)))

(module+ main
  (define n 10000)
  (define avg-degree 10)
  (define graph-file #f)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "mis.rkt"
    #:once-each
    [("--n") arg "Number of vertices (for random graph)"
     (set! n (parse-positive-integer arg 'mis))]
    [("--degree") arg "Average degree (for random graph)"
     (set! avg-degree (parse-positive-integer arg 'mis))]
    [("--graph") arg "Graph input file (edge list format)"
     (set! graph-file arg)]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'mis))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'mis))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'mis))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  ;; Load or generate graph
  (define graph
    (if graph-file
        (begin
          (printf "Loading graph from ~a...\n" graph-file)
          (read-graph-edgelist graph-file))
        (begin
          (printf "Generating random graph (n=~a, avg-degree=~a)...\n" n avg-degree)
          (generate-random-graph n avg-degree seed))))

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

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential MIS...\n")
    (set! seq-result
      (run-benchmark
       (λ () (mis-sequential graph seed))
       #:name 'mis
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel MIS (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (mis-parallel graph workers seed))
     #:name 'mis
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (verify-mis graph result)))
                 (error 'mis "parallel result verification failed at iteration ~a" iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (and (verify-mis graph seq-result) (verify-mis graph par-result))
        (printf "✓ Sequential and parallel results are valid MIS\n")
        (printf "✗ Verification failed!\n")))

  (printf "\nMIS sizes:\n")
  (when seq-result
    (printf "  Sequential: ~a vertices\n" (length seq-result)))
  (printf "  Parallel:   ~a vertices\n" (length par-result))
  (printf "\nNote: MIS sizes may differ between sequential and parallel due to different algorithms\n"))
