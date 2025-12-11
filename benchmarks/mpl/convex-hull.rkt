#lang racket

(require racket/fixnum
         racket/flonum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide convex-hull-sequential
         convex-hull-parallel
         read-points
         verify-convex-hull)

;; ============================================================================
;; Parallel Primitives (inline implementations)
;; ============================================================================

;; Granularity threshold for switching to sequential
(define GRAIN 10000)

;; Parallel reduce with granularity control
;; Returns (values best-idx best-val) where f returns (idx . val)
(define (parallel-reduce-max f n workers)
  (if (< n GRAIN)
      ;; Sequential
      (let loop ([i 0] [best-idx -1] [best-val -inf.0])
        (if (>= i n)
            (values best-idx best-val)
            (let* ([result (f i)]
                   [idx (car result)]
                   [val (cdr result)])
              (if (> val best-val)
                  (loop (add1 i) idx val)
                  (loop (add1 i) best-idx best-val)))))
      ;; Parallel
      (let* ([chunk-size (quotient (+ n workers -1) workers)]
             [channels
              (for/list ([w (in-range workers)])
                (define start (* w chunk-size))
                (define end (min (+ start chunk-size) n))
                (if (>= start n)
                    #f
                    (let ([ch (make-channel)])
                      (thread
                       (lambda ()
                         (let loop ([i start] [best-idx -1] [best-val -inf.0])
                           (if (>= i end)
                               (channel-put ch (cons best-idx best-val))
                               (let* ([result (f i)]
                                      [idx (car result)]
                                      [val (cdr result)])
                                 (if (> val best-val)
                                     (loop (add1 i) idx val)
                                     (loop (add1 i) best-idx best-val)))))))
                      ch)))]
             [results (for/list ([ch channels] #:when ch) (channel-get ch))])
        ;; Merge results
        (for/fold ([best-idx -1] [best-val -inf.0] #:result (values best-idx best-val))
                  ([r results])
          (if (> (cdr r) best-val)
              (values (car r) (cdr r))
              (values best-idx best-val))))))

;; Parallel filter - returns indices that satisfy predicate
(define (parallel-filter pred? n workers)
  (if (< n GRAIN)
      ;; Sequential
      (for/vector ([i (in-range n)] #:when (pred? i)) i)
      ;; Parallel: each worker builds local list, then merge
      (let* ([chunk-size (quotient (+ n workers -1) workers)]
             [channels
              (for/list ([w (in-range workers)])
                (define start (* w chunk-size))
                (define end (min (+ start chunk-size) n))
                (if (>= start n)
                    #f
                    (let ([ch (make-channel)])
                      (thread
                       (lambda ()
                         (channel-put ch
                           (for/list ([i (in-range start end)] #:when (pred? i)) i))))
                      ch)))]
             [results (for/list ([ch channels] #:when ch) (channel-get ch))])
        (list->vector (apply append results)))))

;; Parallel split into left/right based on predicate
;; Returns (values left-indices right-indices)
;; index-fn maps iteration index to actual value to store (usually identity or vector-ref)
(define (parallel-split-indexed left-pred? right-pred? index-fn n workers)
  (if (< n GRAIN)
      ;; Sequential
      (let loop ([i 0] [left '()] [right '()])
        (if (>= i n)
            (values (list->vector (reverse left))
                    (list->vector (reverse right)))
            (let ([val (index-fn i)])
              (cond
                [(left-pred? i) (loop (add1 i) (cons val left) right)]
                [(right-pred? i) (loop (add1 i) left (cons val right))]
                [else (loop (add1 i) left right)]))))
      ;; Parallel
      (let* ([chunk-size (quotient (+ n workers -1) workers)]
             [channels
              (for/list ([w (in-range workers)])
                (define start (* w chunk-size))
                (define end (min (+ start chunk-size) n))
                (if (>= start n)
                    #f
                    (let ([ch (make-channel)])
                      (thread
                       (lambda ()
                         (let loop ([i start] [left '()] [right '()])
                           (if (>= i end)
                               (channel-put ch (cons (reverse left) (reverse right)))
                               (let ([val (index-fn i)])
                                 (cond
                                   [(left-pred? i) (loop (add1 i) (cons val left) right)]
                                   [(right-pred? i) (loop (add1 i) left (cons val right))]
                                   [else (loop (add1 i) left right)]))))))
                      ch)))]
             [results (for/list ([ch channels] #:when ch) (channel-get ch))]
             [lefts (apply append (map car results))]
             [rights (apply append (map cdr results))])
        (values (list->vector lefts) (list->vector rights)))))

;; ============================================================================
;; Point operations
;; ============================================================================

;; Points stored as (vector x y) for cache efficiency
(define (make-point x y) (vector x y))
(define (point-x p) (vector-ref p 0))
(define (point-y p) (vector-ref p 1))

;; Cross product for orientation test
(define (cross-product p q r)
  (fl- (fl* (fl- (point-x q) (point-x p))
            (fl- (point-y r) (point-y p)))
       (fl* (fl- (point-y q) (point-y p))
            (fl- (point-x r) (point-x p)))))

;; Distance from point to line (actually signed area * 2)
(define (signed-area a b p)
  (cross-product a b p))

;; ============================================================================
;; Sequential QuickHull
;; ============================================================================

(define (quickhull-sequential pts indices a b)
  (define n (vector-length indices))
  (if (= n 0)
      '()
      ;; Find farthest point
      (let* ([pt-a (vector-ref pts a)]
             [pt-b (vector-ref pts b)]
             [best-idx -1]
             [best-dist -inf.0])
        (for ([i (in-range n)])
          (define idx (vector-ref indices i))
          (define dist (signed-area pt-a pt-b (vector-ref pts idx)))
          (when (fl> dist best-dist)
            (set! best-idx idx)
            (set! best-dist dist)))
        (if (= best-idx -1)
            '()
            (let* ([pt-mid (vector-ref pts best-idx)]
                   ;; Split into left and right sets
                   [left-indices
                    (for/vector ([i (in-range n)]
                                 #:when (fl> (signed-area pt-a pt-mid
                                              (vector-ref pts (vector-ref indices i))) 0.0))
                      (vector-ref indices i))]
                   [right-indices
                    (for/vector ([i (in-range n)]
                                 #:when (fl> (signed-area pt-mid pt-b
                                              (vector-ref pts (vector-ref indices i))) 0.0))
                      (vector-ref indices i))])
              (append (quickhull-sequential pts left-indices a best-idx)
                      (list best-idx)
                      (quickhull-sequential pts right-indices best-idx b)))))))

(define (convex-hull-sequential points)
  (define pts (if (vector? points) points (list->vector points)))
  (define n (vector-length pts))
  (when (< n 3)
    (error 'convex-hull-sequential "Need at least 3 points"))

  ;; Find leftmost and rightmost points
  (define-values (left-idx right-idx)
    (for/fold ([l 0] [r 0])
              ([i (in-range n)])
      (values (if (fl< (point-x (vector-ref pts i)) (point-x (vector-ref pts l))) i l)
              (if (fl> (point-x (vector-ref pts i)) (point-x (vector-ref pts r))) i r))))

  (define pt-l (vector-ref pts left-idx))
  (define pt-r (vector-ref pts right-idx))

  ;; Split into upper and lower sets
  (define upper-indices
    (for/vector ([i (in-range n)]
                 #:when (fl> (signed-area pt-l pt-r (vector-ref pts i)) 0.0))
      i))
  (define lower-indices
    (for/vector ([i (in-range n)]
                 #:when (fl< (signed-area pt-l pt-r (vector-ref pts i)) 0.0))
      i))

  ;; Compute hull
  (define upper-hull (quickhull-sequential pts upper-indices left-idx right-idx))
  (define lower-hull (quickhull-sequential pts lower-indices right-idx left-idx))

  ;; Convert indices to points
  (append (list (vector-ref pts left-idx))
          (map (lambda (i) (vector-ref pts i)) upper-hull)
          (list (vector-ref pts right-idx))
          (map (lambda (i) (vector-ref pts i)) lower-hull)))

;; ============================================================================
;; Parallel QuickHull (MPL-style)
;; ============================================================================

(define (quickhull-parallel pts indices a b workers threshold)
  (define n (vector-length indices))
  (cond
    [(= n 0) '()]
    [(< n threshold)
     ;; Fall back to sequential for small problems
     (quickhull-sequential pts indices a b)]
    [else
     (define pt-a (vector-ref pts a))
     (define pt-b (vector-ref pts b))

     ;; Parallel reduce to find farthest point
     (define-values (best-idx best-dist)
       (parallel-reduce-max
        (lambda (i)
          (define idx (vector-ref indices i))
          (define dist (signed-area pt-a pt-b (vector-ref pts idx)))
          (cons idx dist))
        n workers))

     (if (= best-idx -1)
         '()
         (let ([pt-mid (vector-ref pts best-idx)])
           ;; Parallel split - pass index-fn to convert i to actual point index
           (define-values (left-indices right-indices)
             (parallel-split-indexed
              (lambda (i)
                (fl> (signed-area pt-a pt-mid (vector-ref pts (vector-ref indices i))) 0.0))
              (lambda (i)
                (fl> (signed-area pt-mid pt-b (vector-ref pts (vector-ref indices i))) 0.0))
              (lambda (i) (vector-ref indices i))
              n workers))

           ;; Fork-join: process left and right in parallel
           (define left-ch (make-channel))
           (thread (lambda ()
                     (channel-put left-ch
                       (quickhull-parallel pts left-indices a best-idx workers threshold))))
           (define right-hull
             (quickhull-parallel pts right-indices best-idx b workers threshold))
           (define left-hull (channel-get left-ch))

           (append left-hull (list best-idx) right-hull)))]))

(define (convex-hull-parallel points workers threshold)
  (define pts (if (vector? points) points (list->vector points)))
  (define n (vector-length pts))
  (when (< n 3)
    (error 'convex-hull-parallel "Need at least 3 points"))

  ;; Parallel reduce to find leftmost and rightmost
  (define-values (left-idx _l)
    (parallel-reduce-max
     (lambda (i) (cons i (fl- 0.0 (point-x (vector-ref pts i)))))  ; negate for min
     n workers))
  (define-values (right-idx _r)
    (parallel-reduce-max
     (lambda (i) (cons i (point-x (vector-ref pts i))))
     n workers))

  (define pt-l (vector-ref pts left-idx))
  (define pt-r (vector-ref pts right-idx))

  ;; Parallel split into upper and lower
  ;; Here i IS the point index, so index-fn is identity
  (define-values (upper-indices lower-indices)
    (parallel-split-indexed
     (lambda (i) (fl> (signed-area pt-l pt-r (vector-ref pts i)) 0.0))
     (lambda (i) (fl< (signed-area pt-l pt-r (vector-ref pts i)) 0.0))
     (lambda (i) i)  ; identity - i is already the point index
     n workers))

  ;; Fork-join for upper and lower hulls
  (define upper-ch (make-channel))
  (thread (lambda ()
            (channel-put upper-ch
              (quickhull-parallel pts upper-indices left-idx right-idx workers threshold))))
  (define lower-hull
    (quickhull-parallel pts lower-indices right-idx left-idx workers threshold))
  (define upper-hull (channel-get upper-ch))

  ;; Convert indices to points
  (append (list (vector-ref pts left-idx))
          (map (lambda (i) (vector-ref pts i)) upper-hull)
          (list (vector-ref pts right-idx))
          (map (lambda (i) (vector-ref pts i)) lower-hull)))

;; ============================================================================
;; I/O and Verification
;; ============================================================================

(define (read-points path)
  (define lines (file->lines path))
  (for/list ([line (in-list lines)])
    (define parts (string-split line))
    (when (< (length parts) 2)
      (error 'read-points "Invalid point format: ~a" line))
    (make-point (string->number (first parts))
                (string->number (second parts)))))

(define (verify-convex-hull points hull #:sample-rate [sample-rate 0.05])
  (define pts (if (vector? points) points (list->vector points)))
  (define hull-vec (list->vector hull))
  (define hull-len (vector-length hull-vec))
  (define n (vector-length pts))

  ;; Check hull is non-empty
  (and (> hull-len 0)
       ;; Sample check: random points should be inside hull
       (let ([sample-count (max 1 (inexact->exact (ceiling (* sample-rate n))))])
         (for/and ([_ (in-range sample-count)])
           (define p (vector-ref pts (random n)))
           ;; Point is inside if on same side of all edges
           (or (for/or ([i (in-range hull-len)])
                 (equal? p (vector-ref hull-vec i)))
               (for/and ([i (in-range hull-len)])
                 (define a (vector-ref hull-vec i))
                 (define b (vector-ref hull-vec (modulo (add1 i) hull-len)))
                 (fl<= (cross-product a b p) 0.0)))))))

;; ============================================================================
;; Point Generation
;; ============================================================================

(define (generate-random-points n seed [distribution 'uniform-circle])
  (random-seed seed)
  (case distribution
    [(uniform-circle)
     (for/vector ([i (in-range n)])
       (define r (flsqrt (random)))
       (define theta (fl* 2.0 (fl* pi (random))))
       (make-point (fl* r (flcos theta)) (fl* r (flsin theta))))]
    [(uniform-square)
     (for/vector ([i (in-range n)])
       (make-point (random) (random)))]
    [else
     (error 'generate-random-points "Unknown distribution: ~a" distribution)]))

;; ============================================================================
;; Main
;; ============================================================================

(module+ main
  (define n 10000)
  (define distribution 'uniform-circle)
  (define points-file #f)
  (define workers (processor-count))
  (define threshold 1000)  ; Lower threshold for better parallelism
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "convex-hull.rkt"
    #:once-each
    [("--n") arg "Number of points"
     (set! n (parse-positive-integer arg 'convex-hull))]
    [("--distribution") arg "Point distribution"
     (set! distribution (string->symbol arg))]
    [("--points") arg "Points input file"
     (set! points-file arg)]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'convex-hull))]
    [("--threshold") arg "Sequential threshold"
     (set! threshold (parse-positive-integer arg 'convex-hull))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'convex-hull))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'convex-hull))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  ;; Load or generate points
  (define points
    (if points-file
        (list->vector (read-points points-file))
        (begin
          (printf "Generating random points (n=~a, distribution=~a)...\n" n distribution)
          (generate-random-points n seed distribution))))

  (define num-points (vector-length points))
  (printf "Number of points: ~a\n" num-points)

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n num-points)
                       (list 'workers workers)
                       (list 'threshold threshold)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential convex hull...\n")
    (set! seq-result
      (run-benchmark
       (lambda () (convex-hull-sequential points))
       #:name 'convex-hull
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel convex hull (workers=~a, threshold=~a)...\n" workers threshold)
  (define par-result
    (run-benchmark
     (lambda () (convex-hull-parallel points workers threshold))
     #:name 'convex-hull
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification:\n")
    (printf "  Sequential hull valid: ~a (~a vertices)\n"
            (verify-convex-hull points seq-result) (length seq-result))
    (printf "  Parallel hull valid: ~a (~a vertices)\n"
            (verify-convex-hull points par-result) (length par-result)))

  (printf "\nHull size: ~a vertices\n" (length par-result)))
