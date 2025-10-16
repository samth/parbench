#lang racket

(require racket/fixnum
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

(provide convex-hull-sequential
         convex-hull-parallel
         read-points
         verify-convex-hull)

;; Point representation: (x . y)
(define (point x y) (cons x y))
(define (point-x p) (car p))
(define (point-y p) (cdr p))

;; Read points from file (format: x y per line)
(define (read-points path)
  (define lines (file->lines path))
  (for/list ([line (in-list lines)])
    (define parts (string-split line))
    (when (< (length parts) 2)
      (error 'read-points "Invalid point format: ~a" line))
    (point (string->number (first parts))
           (string->number (second parts)))))

;; Cross product for orientation test
;; Returns: > 0 if p-q-r is counter-clockwise, < 0 if clockwise, 0 if collinear
(define (cross-product p q r)
  (- (* (- (point-x q) (point-x p))
        (- (point-y r) (point-y p)))
     (* (- (point-y q) (point-y p))
        (- (point-x r) (point-x p)))))

;; Distance from point p to line defined by a and b
(define (distance-to-line p a b)
  (abs (cross-product a b p)))

;; Find the point farthest from line a-b in the given set
(define (find-farthest-point points a b)
  (define max-dist 0)
  (define max-point #f)
  (for ([p (in-list points)])
    (define dist (distance-to-line p a b))
    (when (> dist max-dist)
      (set! max-dist dist)
      (set! max-point p)))
  max-point)

;; Sequential QuickHull algorithm
(define (quickhull-sequential points a b)
  (if (null? points)
      null
      (let ([farthest (find-farthest-point points a b)])
        (if (not farthest)
            null
            (let ([left-set (filter (λ (p) (> (cross-product a farthest p) 0)) points)]
                  [right-set (filter (λ (p) (> (cross-product farthest b p) 0)) points)])
              (append (quickhull-sequential left-set a farthest)
                      (list farthest)
                      (quickhull-sequential right-set farthest b)))))))

(define (convex-hull-sequential points)
  (when (< (length points) 3)
    (error 'convex-hull-sequential "Need at least 3 points"))

  ;; Find leftmost and rightmost points
  (define leftmost (argmin point-x points))
  (define rightmost (argmax point-x points))

  ;; Divide points into upper and lower hulls
  (define upper-set
    (filter (λ (p) (> (cross-product leftmost rightmost p) 0)) points))
  (define lower-set
    (filter (λ (p) (< (cross-product leftmost rightmost p) 0)) points))

  ;; Compute hull
  (append (list leftmost)
          (quickhull-sequential upper-set leftmost rightmost)
          (list rightmost)
          (quickhull-sequential lower-set rightmost leftmost)))

;; Parallel QuickHull algorithm (uses shared thread pool)
(define (quickhull-parallel points a b pool threshold)
  (if (or (null? points) (< (length points) threshold))
      (quickhull-sequential points a b)
      (let ([farthest (find-farthest-point points a b)])
        (if (not farthest)
            null
            (let* ([left-set (filter (λ (p) (> (cross-product a farthest p) 0)) points)]
                   [right-set (filter (λ (p) (> (cross-product farthest b p) 0)) points)]
                   [left-thread (and (not (null? left-set))
                                     (thread-pool-submit
                                      pool
                                      (λ () (quickhull-parallel left-set a farthest pool threshold))))]
                   [right-hull (quickhull-parallel right-set farthest b pool threshold)]
                   [left-hull (if left-thread (thread-pool-wait left-thread) null)])
              (append left-hull (list farthest) right-hull))))))

(define (convex-hull-parallel points workers threshold)
  (when (< (length points) 3)
    (error 'convex-hull-parallel "Need at least 3 points"))

  ;; Find leftmost and rightmost points
  (define leftmost (argmin point-x points))
  (define rightmost (argmax point-x points))

  ;; Divide points into upper and lower hulls
  (define upper-set
    (filter (λ (p) (> (cross-product leftmost rightmost p) 0)) points))
  (define lower-set
    (filter (λ (p) (< (cross-product leftmost rightmost p) 0)) points))

  ;; Compute hull in parallel using shared pool
  (call-with-thread-pool workers
    (λ (pool actual-workers)
      (define upper-thread
        (and (not (null? upper-set))
             (thread-pool-submit
              pool
              (λ () (quickhull-parallel upper-set leftmost rightmost pool threshold)))))
      (define lower-hull (quickhull-parallel lower-set rightmost leftmost pool threshold))
      (define upper-hull (if upper-thread (thread-pool-wait upper-thread) null))
      (append (list leftmost) upper-hull (list rightmost) lower-hull))
    #:max (length points)))

;; Sign function helper
(define (sgn x)
  (cond [(> x 0) 1]
        [(< x 0) -1]
        [else 0]))

;; Verify convex hull correctness
(define (verify-convex-hull points hull)
  ;; Check that all hull points are in the original set
  (define points-set (list->set points))
  (define valid-points?
    (for/and ([p (in-list hull)])
      (set-member? points-set p)))

  ;; Check that all points are either on or inside the hull
  (define hull-len (length hull))
  (define all-inside?
    (for/and ([p (in-list points)])
      ;; Check if point is on hull or on the correct side of all edges
      (or (member p hull)
          (for/and ([i (in-range hull-len)])
            (define a (list-ref hull i))
            (define b (list-ref hull (modulo (add1 i) hull-len)))
            (<= (cross-product a b p) 0)))))

  ;; Check that hull vertices are in consistent order (all CCW or all CW)
  ;; We just check that it forms a valid convex hull, not the specific orientation
  (define consistent-order?
    (if (< hull-len 3)
        #t
        (let ([first-sign (let ([a (list-ref hull 0)]
                               [b (list-ref hull 1)]
                               [c (list-ref hull 2)])
                           (cross-product a b c))])
          (for/and ([i (in-range hull-len)])
            (define a (list-ref hull i))
            (define b (list-ref hull (modulo (add1 i) hull-len)))
            (define c (list-ref hull (modulo (+ i 2) hull-len)))
            (define cp (cross-product a b c))
            ;; All should have the same sign as first-sign (or be zero)
            (or (= cp 0)
                (= (sgn first-sign) (sgn cp)))))))

  (and valid-points? all-inside? consistent-order?))

;; Generate random points
(define (generate-random-points n seed [distribution 'uniform-circle])
  (random-seed seed)
  (case distribution
    [(uniform-circle)
     ;; Uniform distribution within unit circle
     (for/list ([i (in-range n)])
       (define r (sqrt (random)))
       (define theta (* 2 pi (random)))
       (point (* r (cos theta)) (* r (sin theta))))]
    [(uniform-square)
     ;; Uniform distribution in unit square
     (for/list ([i (in-range n)])
       (point (random) (random)))]
    [(circle-perimeter)
     ;; Uniform distribution on circle perimeter
     (for/list ([i (in-range n)])
       (define theta (* 2 pi (random)))
       (point (cos theta) (sin theta)))]
    [else
     (error 'generate-random-points "Unknown distribution: ~a" distribution)]))

(module+ main
  (define n 10000)
  (define distribution 'uniform-circle)
  (define points-file #f)
  (define workers (processor-count))
  (define threshold 100)
  (define repeat 3)
  (define log-path #f)
  (define seed 42)

  (void
   (command-line
    #:program "convex-hull.rkt"
    #:once-each
    [("--n") arg "Number of points (for random generation)"
     (set! n (parse-positive-integer arg 'convex-hull))]
    [("--distribution") arg "Point distribution: uniform-circle, uniform-square, circle-perimeter"
     (set! distribution (string->symbol arg))]
    [("--points") arg "Points input file"
     (set! points-file arg)]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'convex-hull))]
    [("--threshold") arg "Sequential threshold for parallel algorithm"
     (set! threshold (parse-positive-integer arg 'convex-hull))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'convex-hull))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'convex-hull))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]))

  ;; Load or generate points
  (define points
    (if points-file
        (begin
          (printf "Loading points from ~a...\n" points-file)
          (read-points points-file))
        (begin
          (printf "Generating random points (n=~a, distribution=~a)...\n" n distribution)
          (generate-random-points n seed distribution))))

  (define num-points (length points))
  (printf "Number of points: ~a\n" num-points)

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n num-points)
                       (list 'workers workers)
                       (list 'threshold threshold)
                       (list 'seed seed)))

  (printf "Running sequential convex hull (QuickHull)...\n")
  (define seq-result
    (run-benchmark
     (λ () (convex-hull-sequential points))
     #:name 'convex-hull
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (printf "Running parallel convex hull (workers=~a, threshold=~a)...\n" workers threshold)
  (define par-result
    (run-benchmark
     (λ () (convex-hull-parallel points workers threshold))
     #:name 'convex-hull
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (unless (verify-convex-hull points result)
                 (error 'convex-hull "parallel result verification failed at iteration ~a" iteration)))))

  (close-log-writer writer)

  (printf "\nVerification: ")
  (if (and (verify-convex-hull points seq-result) (verify-convex-hull points par-result))
      (printf "✓ Sequential and parallel results are valid convex hulls\n")
      (printf "✗ Verification failed!\n"))

  (printf "\nConvex Hull Statistics:\n")
  (printf "  Input points: ~a\n" num-points)
  (printf "  Sequential hull: ~a vertices\n" (length seq-result))
  (printf "  Parallel hull:   ~a vertices\n" (length par-result))

  (printf "\nSample hull vertices (first 5 from sequential):\n")
  (for ([p (in-list (take seq-result (min 5 (length seq-result))))])
    (printf "  (~a, ~a)\n" (point-x p) (point-y p))))
