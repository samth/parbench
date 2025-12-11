#lang racket

(require racket/fixnum
         racket/vector
         racket/list
         racket/set
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide convex-hull-sequential
         convex-hull-parallel
         read-points
         verify-convex-hull)

;; Point representation: (x . y)
(define (point x y) (cons x y))
(define (point-x p) (car p))
(define (point-y p) (cdr p))

;; Optional detailed timing for algorithm/verification.
(define timing-enabled? (make-parameter #f))

(define (with-timing label thunk)
  (define start (current-inexact-milliseconds))
  (define v (thunk))
  (define dt (- (current-inexact-milliseconds) start))
  (when (timing-enabled?)
    (printf "  [time] ~a: ~a ms\n" label (inexact->exact (round dt))))
  v)

;; Normalize points input to a vector.
(define (points->vector points)
  (cond
    [(vector? points) points]
    [(list? points) (list->vector points)]
    [else (error 'convex-hull "expected list or vector of points, got ~a" points)]))

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
(define (find-farthest-point points-vec a b)
  (define max-dist 0)
  (define max-point #f)
  (for ([p (in-vector points-vec)])
    (define dist (distance-to-line p a b))
    (when (> dist max-dist)
      (set! max-dist dist)
      (set! max-point p)))
  max-point)

;; Filter a vector by predicate, returning a vector.
(define (vector-filter pred vec)
  (for/vector ([p (in-vector vec)] #:when (pred p)) p))

;; Argmin/argmax over vectors.
(define (argmin-vector key vec)
  (when (zero? (vector-length vec))
    (error 'argmin-vector "empty vector"))
  (define best (vector-ref vec 0))
  (define best-k (key best))
  (for ([i (in-range 1 (vector-length vec))])
    (define p (vector-ref vec i))
    (define k (key p))
    (when (< k best-k)
      (set! best p)
      (set! best-k k)))
  best)

(define (argmax-vector key vec)
  (when (zero? (vector-length vec))
    (error 'argmax-vector "empty vector"))
  (define best (vector-ref vec 0))
  (define best-k (key best))
  (for ([i (in-range 1 (vector-length vec))])
    (define p (vector-ref vec i))
    (define k (key p))
    (when (> k best-k)
      (set! best p)
      (set! best-k k)))
  best)

;; Sequential QuickHull algorithm
(define (quickhull-sequential points-vec a b)
  (if (zero? (vector-length points-vec))
      null
      (let ([farthest (find-farthest-point points-vec a b)])
        (if (not farthest)
            null
            (let ([left-set (vector-filter (λ (p) (> (cross-product a farthest p) 0)) points-vec)]
                  [right-set (vector-filter (λ (p) (> (cross-product farthest b p) 0)) points-vec)])
              (append (quickhull-sequential left-set a farthest)
                      (list farthest)
                      (quickhull-sequential right-set farthest b)))))))

(define (convex-hull-sequential points)
  (define points-vec (points->vector points))
  (when (< (vector-length points-vec) 3)
    (error 'convex-hull-sequential "Need at least 3 points"))

  ;; Find leftmost and rightmost points
  (define leftmost (argmin-vector point-x points-vec))
  (define rightmost (argmax-vector point-x points-vec))

  ;; Divide points into upper and lower hulls
  (define upper-set
    (vector-filter (λ (p) (> (cross-product leftmost rightmost p) 0)) points-vec))
  (define lower-set
    (vector-filter (λ (p) (< (cross-product leftmost rightmost p) 0)) points-vec))

  ;; Compute hull
  (with-timing "sequential-hull-total"
    (λ ()
      (append (list leftmost)
              (with-timing "sequential-upper"
                (λ () (quickhull-sequential upper-set leftmost rightmost)))
              (list rightmost)
              (with-timing "sequential-lower"
                (λ () (quickhull-sequential lower-set rightmost leftmost)))))))

;; Parallel QuickHull algorithm using channels for communication
(define (quickhull-parallel points-vec a b threshold)
  (if (or (zero? (vector-length points-vec))
          (< (vector-length points-vec) threshold))
      (quickhull-sequential points-vec a b)
      (let ([farthest (find-farthest-point points-vec a b)])
        (if (not farthest)
            null
            (let* ([left-set (vector-filter (λ (p) (> (cross-product a farthest p) 0)) points-vec)]
                   [right-set (vector-filter (λ (p) (> (cross-product farthest b p) 0)) points-vec)]
                   [left-ch (and (positive? (vector-length left-set))
                                 (let ([ch (make-channel)])
                                   (thread (λ () (channel-put ch (quickhull-parallel left-set a farthest threshold))))
                                   ch))]
                   [right-hull (quickhull-parallel right-set farthest b threshold)]
                   [left-hull (if left-ch (channel-get left-ch) null)])
              (append left-hull (list farthest) right-hull))))))

(define (convex-hull-parallel points workers threshold)
  (define points-vec (points->vector points))
  (when (< (vector-length points-vec) 3)
    (error 'convex-hull-parallel "Need at least 3 points"))

  ;; Find leftmost and rightmost points
  (define leftmost (argmin-vector point-x points-vec))
  (define rightmost (argmax-vector point-x points-vec))

  ;; Divide points into upper and lower hulls
  (define upper-set
    (vector-filter (λ (p) (> (cross-product leftmost rightmost p) 0)) points-vec))
  (define lower-set
    (vector-filter (λ (p) (< (cross-product leftmost rightmost p) 0)) points-vec))

  ;; Compute hull in parallel using threads and channels
  (define upper-ch
    (and (positive? (vector-length upper-set))
         (let ([ch (make-channel)])
           (thread (λ () (channel-put ch (quickhull-parallel upper-set leftmost rightmost threshold))))
           ch)))
  (define lower-hull (quickhull-parallel lower-set rightmost leftmost threshold))
  (define upper-hull (if upper-ch (channel-get upper-ch) null))
  (with-timing "parallel-hull-total"
    (λ () (append (list leftmost) upper-hull (list rightmost) lower-hull))))

;; Sign function helper
(define (sgn x)
  (cond [(> x 0) 1]
        [(< x 0) -1]
        [else 0]))

;; Verify convex hull correctness
(define (verify-convex-hull points hull #:sample-rate [sample-rate 0.05])
  (define points-vec (points->vector points))
  (define hull-vec (points->vector hull))
  (define hull-len (vector-length hull-vec))

  ;; Build sets for fast membership.
  (define points-set
    (with-timing "verify-build-points-set"
      (λ () (for/set ([p (in-vector points-vec)]) p))))
  (define hull-set
    (with-timing "verify-build-hull-set"
      (λ () (for/set ([p (in-vector hull-vec)]) p))))

  ;; Check that all hull points are in the original set.
  (define valid-points?
    (with-timing "verify-hull-points-in-input"
      (λ ()
        (for/and ([p (in-vector hull-vec)])
          (set-member? points-set p)))))

  ;; Sample 5% of points (with replacement) for inside checks.
  (define num-points (vector-length points-vec))
  (define sample-count
    (max 1 (inexact->exact (ceiling (* sample-rate num-points)))))
  (define sampled-points
    (with-timing "verify-sample-points"
      (λ ()
        (for/list ([i (in-range sample-count)])
          (vector-ref points-vec (random num-points))))))

  (define (inside-or-on-hull? p)
    (or (set-member? hull-set p)
        (for/and ([i (in-range hull-len)])
          (define a (vector-ref hull-vec i))
          (define b (vector-ref hull-vec (if (= i (sub1 hull-len)) 0 (add1 i))))
          (<= (cross-product a b p) 0))))

  (define all-inside?
    (with-timing "verify-sampled-inside-check"
      (λ ()
        (for/and ([p (in-list sampled-points)])
          (inside-or-on-hull? p)))))

  ;; Check that hull vertices are in consistent order (all CCW or all CW).
  (define consistent-order?
    (with-timing "verify-consistent-order"
      (λ ()
        (if (< hull-len 3)
            #t
            (let* ([a0 (vector-ref hull-vec 0)]
                   [b0 (vector-ref hull-vec 1)]
                   [c0 (vector-ref hull-vec 2)]
                   [first-sign (cross-product a0 b0 c0)])
              (for/and ([i (in-range hull-len)])
                (define a (vector-ref hull-vec i))
                (define b (vector-ref hull-vec (modulo (add1 i) hull-len)))
                (define c (vector-ref hull-vec (modulo (+ i 2) hull-len)))
                (define cp (cross-product a b c))
                (or (= cp 0)
                    (= (sgn first-sign) (sgn cp)))))))))

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
  (define skip-sequential #f)
  (define show-timings #f)

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
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]
    [("--timings") "Print detailed step timings"
     (set! show-timings #t)]))

  ;; Load or generate points
  (define points
    (with-timing "load-or-generate-points"
      (λ ()
        (if points-file
            (begin
              (printf "Loading points from ~a...\n" points-file)
              (read-points points-file))
            (begin
              (printf "Generating random points (n=~a, distribution=~a)...\n" n distribution)
              (generate-random-points n seed distribution))))))

  (define points-vec (points->vector points))
  (define num-points (vector-length points-vec))
  (printf "Number of points: ~a\n" num-points)

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n num-points)
                       (list 'workers workers)
                       (list 'threshold threshold)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential convex hull (QuickHull)...\n")
    (set! seq-result
      (run-benchmark
       (λ () (parameterize ([timing-enabled? #f])
              (convex-hull-sequential points-vec)))
       #:name 'convex-hull
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel convex hull (workers=~a, threshold=~a)...\n" workers threshold)
  (define par-result
    (run-benchmark
     (λ () (parameterize ([timing-enabled? #f])
            (convex-hull-parallel points-vec workers threshold)))
     #:name 'convex-hull
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result
                          (not (parameterize ([timing-enabled? #f])
                                 (verify-convex-hull points-vec result))))
                 (error 'convex-hull "parallel result verification failed at iteration ~a" iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification (random 5% sample):\n")
    (define verified?
      (parameterize ([timing-enabled? show-timings])
        (and (with-timing "verify-sequential"
               (λ () (verify-convex-hull points-vec seq-result)))
             (with-timing "verify-parallel"
               (λ () (verify-convex-hull points-vec par-result))))))
    (printf "  Result: ~a\n" (if verified? "✓ valid convex hulls" "✗ verification failed")))

  (printf "\nConvex Hull Statistics:\n")
  (printf "  Input points: ~a\n" num-points)
  (when seq-result
    (printf "  Sequential hull: ~a vertices\n" (length seq-result)))
  (printf "  Parallel hull:   ~a vertices\n" (length par-result))

  (printf "\nSample hull vertices (first 5 from parallel):\n")
  (for ([p (in-list (take par-result (min 5 (length par-result))))])
    (printf "  (~a, ~a)\n" (point-x p) (point-y p))))
