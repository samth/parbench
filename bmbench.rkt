#lang racket
(require racket/future         ; processor-count
         racket/match
         racket/string)

(provide vector-boyer-moore-majority/sequential
         vector-boyer-moore-majority/parallel
         make-majority-vector
         equal/heavy)

;; -------- Tunable size --------
(define N 1000000) ; change here

;; -------- Boyer–Moore (vector-only, multiple values) --------

(define (bm-merge c1 n1 c2 n2 equal)
  (cond [(and c1 c2 (equal c1 c2)) (values c1 (+ n1 n2))]
        [(>= n1 n2)                (values c1 (- n1 n2))]
        [else                      (values c2 (- n2 n1))]))

(define (bm-scan-vec v start end equal)
  (for/fold ([cand #f] [cnt 0]) ([i (in-range start end)])
    (define x (vector-ref v i))
    (cond [(zero? cnt)    (values x 1)]
          [(equal x cand) (values cand (add1 cnt))]
          [else           (values cand (sub1 cnt))])))

(define (bm-verify-vec v cand equal)
  (and cand
       (> (for/sum ([i (in-range (vector-length v))])
            (if (equal (vector-ref v i) cand) 1 0))
          (quotient (vector-length v) 2))
       cand))

(define (chunk-ranges n k)
  (define k* (max 1 (min k n)))
  (define base (quotient n k*))
  (define rem  (remainder n k*))
  (for/list ([i (in-range k*)])
    (define start (+ (* i base) (min i rem)))
    (define end   (+ start base (if (< i rem) 1 0)))
    (cons start end)))

(define (pair-up xs)
  (let loop ([xs xs] [acc '()])
    (cond [(null? xs)       (reverse acc)]
          [(null? (cdr xs)) (reverse (cons (list (car xs)) acc))]
          [else             (loop (cddr xs) (cons (list (car xs) (cadr xs)) acc))])))

;; -------- Parallel scan + parallel tree-merge (thread #:pool) --------

(define (vector-boyer-moore-majority/parallel v
          #:workers [workers (processor-count)]
          #:pool    [pool    #f]
          #:equal   [equal   equal?])
  (unless (vector? v) (error 'vector-boyer-moore-majority/parallel "vector required"))
  (define n (vector-length v))
  (cond
    [(zero? n) #f]
    [else
     (define k (max 1 (min workers n)))
     (define p (or pool (make-parallel-thread-pool k)))

     ;; leaf scans
     (define scans
       (for/list ([rg (in-list (chunk-ranges n k))])
         (define start (car rg)) (define end (cdr rg))
         (thread (λ () (bm-scan-vec v start end equal))
                 #:pool p #:keep 'results)))

     ;; parallel tree merge
     (define (tree-merge threads)
       (cond
         [(null? threads) (values #f 0)]
         [(null? (cdr threads)) (thread-wait (car threads))]
         [else
          (define next-level
            (for/list ([pr (in-list (pair-up threads))])
              (match pr
                [(list t1 t2)
                 (thread (λ ()
                           (define-values (c1 n1) (thread-wait t1))
                           (define-values (c2 n2) (thread-wait t2))
                           (bm-merge c1 n1 c2 n2 equal))
                         #:pool p #:keep 'results)]
                [(list t1) t1])))
          (tree-merge next-level)]))

     (define-values (cand cnt) (tree-merge scans))
     (unless pool (parallel-thread-pool-close p))
     (bm-verify-vec v cand equal)]))

;; -------- Sequential baseline --------

(define (vector-boyer-moore-majority/sequential v #:equal [equal equal?])
  (define-values (c n) (bm-scan-vec v 0 (vector-length v) equal))
  (bm-verify-vec v c equal))

;; -------- Work generator and heavy equality --------

(define (equal/heavy a b) ; CPU-heavy comparison
  (define (f x)
    (let loop ([k 20] [h (bitwise-xor x #x9e3779b9)])
      (if (zero? k) h
          (loop (sub1 k)
                (bitwise-xor (arithmetic-shift h 1)
                              (* 1103515245 (bitwise-and (+ h x) #xffffffff)))))))
  (= (f a) (f b)))

(define (make-majority-vector n maj #:p [p 0.6] #:kinds [k 32])
  (define m (inexact->exact (round (* p n))))
  (define v (make-vector n 0))
  (for ([i (in-range m)]) (vector-set! v i maj))
  (for ([i (in-range m n)])
    (let loop () (define r (random k))
      (if (= r maj) (loop) (vector-set! v i r))))
  ;; shuffle
  (for ([i (in-range n)])
    (define j (random n))
    (define tmp (vector-ref v i))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j tmp))
  v)

;; -------- Benchmark --------

(module+ main
  (define benchmark-sizes-default '(10000 50000 200000 1000000))
  (define benchmark-sizes benchmark-sizes-default)
  (define worker-counts '(1 2 4 8))
  (define target-work N)
  (define majority-value 7)
  (define majority-probability 0.6)
  (define majority-kinds 64)
  (define rng-seed 424242)

  (define (parse-positive-integer str label)
    (define n (string->number str))
    (unless (and n (integer? n) (> n 0))
      (error 'bmbench-main "~a expects a positive integer, got ~a" label str))
    (inexact->exact n))

  (define (parse-integer str label)
    (define n (string->number str))
    (unless (and n (integer? n))
      (error 'bmbench-main "~a expects an integer, got ~a" label str))
    (inexact->exact n))

  (define (parse-positive-list str label)
    (define parts
      (for/list ([piece (in-list (string-split str "," #:trim? #f))]
                 #:unless (string=? "" (string-trim piece)))
        (string-trim piece)))
    (unless (pair? parts)
      (error 'bmbench-main "~a expects at least one integer" label))
    (for/list ([piece (in-list parts)])
      (parse-positive-integer piece label)))

  (define (parse-probability str)
    (define n (string->number str))
    (unless (and n (real? n) (> n 0) (<= n 1))
      (error 'bmbench-main "--probability expects a real in (0,1], got ~a" str))
    (exact->inexact n))

  (define (maybe-parse-seed str)
    (define lowered (string-downcase (string-trim str)))
    (if (or (string=? lowered "none") (string=? lowered "null"))
        #f
        (parse-integer str "--seed")))

  (command-line
   #:program "bmbench.rkt"
   #:once-each
   [("--sizes") arg "Comma-separated vector lengths (e.g., 10000,50000)."
    (set! benchmark-sizes (parse-positive-list arg "--sizes"))]
   [("--workers") arg "Comma-separated worker counts for benchmarking."
    (set! worker-counts (parse-positive-list arg "--workers"))]
   [("--target-work") arg "Approximate total element work per size (default mirrors N)."
    (set! target-work (parse-positive-integer arg "--target-work"))]
   [("--majority") arg "Majority value inserted into generated vectors."
    (set! majority-value (parse-integer arg "--majority"))]
   [("--probability") arg "Majority probability between 0 and 1 (default 0.6)."
    (set! majority-probability (parse-probability arg))]
   [("--kinds") arg "Distinct value kinds allocated to non-majority elements."
    (set! majority-kinds (parse-positive-integer arg "--kinds"))]
   [("--seed") arg "Random seed integer, or 'none' to leave RNG state untouched."
    (set! rng-seed (maybe-parse-seed arg))])

  (unless (pair? benchmark-sizes)
    (error 'bmbench-main "Benchmark sizes list may not be empty."))
  (unless (pair? worker-counts)
    (error 'bmbench-main "Worker count list may not be empty."))

  (when rng-seed (random-seed rng-seed))

  (define (iterations-for len)
    (max 1 (exact-round (/ target-work len))))

  (define (bench label thunk #:repeat [repeat 1] #:check [check #f])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (displayln (format "~a (x~a)" label repeat))
    (time
     (let loop ([i repeat] [last #f])
       (if (zero? i)
           last
           (let ([res (thunk)])
             (when check (check res))
             (loop (sub1 i) res))))))

  (for ([len (in-list benchmark-sizes)]
        [index (in-naturals)])
    (define repeats (iterations-for len))
    (define vec (make-majority-vector len majority-value
                                      #:p majority-probability
                                      #:kinds majority-kinds))
    (when (> index 0) (newline))
    (displayln (format "=== vector length ~a, repeat x~a ===" len repeats))

    (define baseline #f)
    (bench (format "sequential size ~a" len)
           (λ () (vector-boyer-moore-majority/sequential vec #:equal equal/heavy))
           #:repeat repeats
           #:check (λ (res)
                     (if baseline
                         (unless (equal? res baseline)
                           (error 'benchmark
                                  "Sequential result changed for size ~a" len))
                         (set! baseline res))))

    (for ([w (in-list worker-counts)])
      (define pool (make-parallel-thread-pool w))
      (bench (format "parallel size ~a worker(s) ~a" len w)
             (λ () (vector-boyer-moore-majority/parallel vec
                                                         #:pool pool
                                                         #:equal equal/heavy))
             #:repeat repeats
             #:check (λ (res)
                       (unless (equal? res baseline)
                         (error 'benchmark
                                "Parallel result mismatch for size ~a worker(s) ~a"
                                len w))))
      (parallel-thread-pool-close pool)))
  (newline))
