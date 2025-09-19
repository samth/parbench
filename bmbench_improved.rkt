#lang racket

(require racket/future
         racket/match
         racket/string)

(provide vector-boyer-moore-majority/sequential
         vector-boyer-moore-majority/parallel/improved
         make-majority-vector
         equal/heavy)

;; -------- Tunable size (default for CLI) --------
(define DEFAULT-N 1000000)

;; -------- Boyer–Moore Helpers --------

(define (bm-merge c1 n1 c2 n2 equal)
  (cond
    [(and c1 c2 (equal c1 c2)) (values c1 (+ n1 n2))]
    [(>= n1 n2) (values c1 (- n1 n2))]
    [else (values c2 (- n2 n1))]))

(define (bm-scan-vec v start end equal)
  (for/fold ([cand #f] [cnt 0]) ([i (in-range start end)])
    (define x (vector-ref v i))
    (cond
      [(zero? cnt) (values x 1)]
      [(equal x cand) (values cand (add1 cnt))]
      [else (values cand (sub1 cnt))])))

(define (bm-verify-vec v cand equal)
  (and cand
       (> (for/sum ([i (in-range (vector-length v))])
            (if (equal (vector-ref v i) cand) 1 0))
          (quotient (vector-length v) 2))
       cand))

(define (chunk-ranges n k)
  (define k* (max 1 (min k n)))
  (define base (quotient n k*))
  (define rem (remainder n k*))
  (for/list ([i (in-range k*)])
    (define start (+ (* i base) (min i rem)))
    (define end (+ start base (if (< i rem) 1 0)))
    (cons start end)))

;; -------- Sequential Baseline --------

(define (vector-boyer-moore-majority/sequential v #:equal [equal equal?])
  (define-values (cand cnt) (bm-scan-vec v 0 (vector-length v) equal))
  (bm-verify-vec v cand equal))

;; -------- Improved Parallel Implementation --------

(define (compute-chunk-count n workers chunk-size chunk-multiplier threshold)
  (define effective-workers (max 1 (min workers n)))
  (define desired-size (max 1 (or chunk-size (max threshold (ceiling (/ n effective-workers))))))
  (define raw-count (max 1 (ceiling (/ n desired-size))))
  (define max-chunks (max effective-workers (min n (* effective-workers (max 1 chunk-multiplier)))))
  (min (max effective-workers raw-count) max-chunks))

(define (vector-boyer-moore-majority/parallel/improved v
          #:workers [workers (processor-count)]
          #:equal [equal equal?]
          #:threshold [threshold 50000]
          #:chunk-size [chunk-size #f]
          #:chunk-multiplier [chunk-multiplier 2])
  (unless (vector? v)
    (error 'vector-boyer-moore-majority/parallel/improved "vector required"))
  (define n (vector-length v))
  (cond
    [(zero? n) #f]
    [(<= n threshold)
     (vector-boyer-moore-majority/sequential v #:equal equal)]
    [else
     (define chunk-count (compute-chunk-count n workers chunk-size chunk-multiplier threshold))
     (define ranges (chunk-ranges n chunk-count))
     (define scans
       (for/list ([rg (in-list ranges)])
         (define start (car rg))
         (define end (cdr rg))
         (future (λ () (bm-scan-vec v start end equal)))))
     (define-values (cand cnt)
       (let loop ([fs scans] [cand #f] [cnt 0])
         (cond
           [(null? fs) (values cand cnt)]
           [else
            (define-values (c n) (touch (car fs)))
            (define-values (new-c new-n)
              (if cand
                  (bm-merge cand cnt c n equal)
                  (values c n)))
            (loop (cdr fs) new-c new-n)])))
     (bm-verify-vec v cand equal)]))

;; -------- Work generator and heavy equality --------

(define (equal/heavy a b)
  (define (f x)
    (let loop ([k 20] [h (bitwise-xor x #x9e3779b9)])
      (if (zero? k)
          h
          (loop (sub1 k)
                (bitwise-xor (arithmetic-shift h 1)
                              (* 1103515245 (bitwise-and (+ h x) #xffffffff)))))))
  (= (f a) (f b)))

(define (make-majority-vector n maj #:p [p 0.6] #:kinds [k 32])
  (define m (inexact->exact (round (* p n))))
  (define v (make-vector n 0))
  (for ([i (in-range m)]) (vector-set! v i maj))
  (for ([i (in-range m n)])
    (let loop ()
      (define r (random k))
      (if (= r maj)
          (loop)
          (vector-set! v i r))))
  (for ([i (in-range n)])
    (define j (random n))
    (define tmp (vector-ref v i))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j tmp))
  v)

;; -------- Benchmark Harness --------

(module+ main
  (define benchmark-sizes '(10000 50000 200000 1000000))
  (define worker-counts '(1 2 4 8))
  (define target-work DEFAULT-N)
  (define majority-value 7)
  (define majority-probability 0.6)
  (define majority-kinds 64)
  (define rng-seed 424242)
  (define threshold 50000)
  (define chunk-size #f)
  (define chunk-multiplier 2)

  (define (parse-positive-integer str label)
    (define n (string->number str))
    (unless (and n (integer? n) (> n 0))
      (error 'bmbench-improved "~a expects a positive integer, got ~a" label str))
    (inexact->exact n))

  (define (parse-integer str label)
    (define n (string->number str))
    (unless (and n (integer? n))
      (error 'bmbench-improved "~a expects an integer, got ~a" label str))
    (inexact->exact n))

  (define (parse-positive-list str label)
    (define parts
      (for/list ([piece (in-list (string-split str "," #:trim? #f))]
                 #:unless (string=? "" (string-trim piece)))
        (string-trim piece)))
    (unless (pair? parts)
      (error 'bmbench-improved "~a expects at least one integer" label))
    (for/list ([piece (in-list parts)])
      (parse-positive-integer piece label)))

  (define (parse-probability str)
    (define n (string->number str))
    (unless (and n (real? n) (> n 0) (<= n 1))
      (error 'bmbench-improved "--probability expects a real in (0,1], got ~a" str))
    (exact->inexact n))

  (define (maybe-parse-seed str)
    (define lowered (string-downcase (string-trim str)))
    (if (or (string=? lowered "none") (string=? lowered "null"))
        #f
        (parse-integer str "--seed")))

  (command-line
   #:program "bmbench_improved.rkt"
   #:once-each
   [("--sizes") arg "Comma-separated vector lengths (e.g., 10000,50000)."
    (set! benchmark-sizes (parse-positive-list arg "--sizes"))]
   [("--workers") arg "Comma-separated worker counts for benchmarking."
    (set! worker-counts (parse-positive-list arg "--workers"))]
   [("--target-work") arg "Approximate element work per size (default mirrors DEFAULT-N)."
    (set! target-work (parse-positive-integer arg "--target-work"))]
   [("--majority") arg "Majority value inserted into generated vectors."
    (set! majority-value (parse-integer arg "--majority"))]
   [("--probability") arg "Majority probability between 0 and 1 (default 0.6)."
    (set! majority-probability (parse-probability arg))]
   [("--kinds") arg "Distinct kinds allocated to non-majority elements."
    (set! majority-kinds (parse-positive-integer arg "--kinds"))]
   [("--seed") arg "Random seed integer, or 'none' to leave RNG state untouched."
    (set! rng-seed (maybe-parse-seed arg))]
   [("--threshold") arg "Vector length threshold below which runs stay sequential."
    (set! threshold (parse-positive-integer arg "--threshold"))]
   [("--chunk-size") arg "Preferred chunk size per future (default auto)."
    (set! chunk-size (if (or (string-ci=? arg "auto") (string-ci=? arg "none"))
                         #f
                         (parse-positive-integer arg "--chunk-size")))]
   [("--chunk-multiplier") arg "Upper bound multiplier for chunks relative to workers."
    (set! chunk-multiplier (parse-positive-integer arg "--chunk-multiplier"))])

  (unless (pair? benchmark-sizes)
    (error 'bmbench-improved "Benchmark sizes list may not be empty."))
  (unless (pair? worker-counts)
    (error 'bmbench-improved "Worker count list may not be empty."))

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
                           (error 'bmbench-improved
                                  "Sequential result changed for size ~a" len))
                         (set! baseline res))))

    (for ([w (in-list worker-counts)])
      (bench (format "parallel improved size ~a worker(s) ~a" len w)
             (λ () (vector-boyer-moore-majority/parallel/improved
                    vec
                    #:workers w
                    #:equal equal/heavy
                    #:threshold threshold
                    #:chunk-size chunk-size
                    #:chunk-multiplier chunk-multiplier))
             #:repeat repeats
             #:check (λ (res)
                       (unless (equal? res baseline)
                         (error 'bmbench-improved
                                "Parallel result mismatch for size ~a worker(s) ~a"
                                len w))))))
  (newline))
