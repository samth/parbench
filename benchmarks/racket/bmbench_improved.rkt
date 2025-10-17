#lang racket

(require racket/place
         racket/match
         racket/string
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt")

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
     (define-values (cand cnt)
       (call-with-thread-pool workers
         (λ (pool actual-workers)
           (define results
             (thread-pool-wait/collect
              (for/list ([rg (in-list ranges)])
                (define start (car rg))
                (define end (cdr rg))
                (thread-pool-submit pool (λ () (bm-scan-vec v start end equal))))))
           (let loop ([vals results] [cand #f] [cnt 0])
             (cond
               [(null? vals) (values cand cnt)]
               [else
                (define pair (car vals))
                (define c (first pair))
                (define n (second pair))
                (define-values (next-c next-n)
                  (if cand
                      (bm-merge cand cnt c n equal)
                      (values c n)))
                (loop (cdr vals) next-c next-n)])))
         #:max chunk-count))
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
  (define log-path #f)
  (define repeat-override #f)

  (void
   (command-line
   #:program "bmbench_improved.rkt"
   #:once-each
   [("--sizes") arg "Comma-separated vector lengths (e.g., 10000,50000)."
    (set! benchmark-sizes (parse-positive-list arg 'bmbench-improved))]
   [("--workers") arg "Comma-separated worker counts for benchmarking."
    (set! worker-counts (parse-positive-list arg 'bmbench-improved))]
   [("--target-work") arg "Approximate element work per size (default mirrors DEFAULT-N)."
    (set! target-work (parse-positive-integer arg 'bmbench-improved))]
   [("--majority") arg "Majority value inserted into generated vectors."
    (set! majority-value (parse-integer arg 'bmbench-improved))]
   [("--probability") arg "Majority probability between 0 and 1 (default 0.6)."
    (set! majority-probability (parse-probability arg 'bmbench-improved))]
   [("--kinds") arg "Distinct kinds allocated to non-majority elements."
    (set! majority-kinds (parse-positive-integer arg 'bmbench-improved))]
   [("--seed") arg "Random seed integer, or 'none' to leave RNG state untouched."
    (set! rng-seed (maybe-parse-seed arg 'bmbench-improved))]
   [("--threshold") arg "Vector length threshold below which runs stay sequential."
    (set! threshold (parse-positive-integer arg 'bmbench-improved))]
   [("--chunk-size") arg "Preferred chunk size per worker (default auto)."
    (set! chunk-size (if (or (string-ci=? arg "auto") (string-ci=? arg "none"))
                         #f
                         (parse-positive-integer arg 'bmbench-improved)))]
   [("--chunk-multiplier") arg "Upper bound multiplier for chunks relative to workers."
    (set! chunk-multiplier (parse-positive-integer arg 'bmbench-improved))]
   [("--repeat") arg "Override per-size benchmark repeat count."
    (set! repeat-override (parse-positive-integer arg 'bmbench-improved))]
   [("--log") arg "Optional path for S-expression benchmark log."
    (set! log-path arg)]))

  (let ()
    (when rng-seed (random-seed rng-seed))

    (define writer (make-log-writer log-path))
    (define metadata (system-metadata))

    (define (iterations-for len)
      (or repeat-override
          (max 1 (exact-round (/ target-work len)))))

    (define (base-params len repeats)
      (list (list 'size len)
            (list 'repeats repeats)
            (list 'majority majority-value)
            (list 'probability majority-probability)
            (list 'kinds majority-kinds)
            (list 'threshold threshold)
            (list 'chunk-size (or chunk-size 'auto))
            (list 'chunk-multiplier chunk-multiplier)))

    (define sequential-result #f)

    (for ([len (in-list benchmark-sizes)]
          [index (in-naturals)])
      (define repeats (iterations-for len))
      (define vec (make-majority-vector len majority-value
                                        #:p majority-probability
                                        #:kinds majority-kinds))
      (when (> index 0) (newline))
      (displayln (format "=== vector length ~a, repeat x~a ===" len repeats))

      (set! sequential-result
            (run-benchmark
             (λ () (vector-boyer-moore-majority/sequential vec #:equal equal/heavy))
             #:name 'bmbench-improved
             #:variant 'sequential
             #:repeat repeats
             #:log-writer writer
             #:params (base-params len repeats)
             #:metadata metadata
             #:check (let ([first #f])
                       (λ (iteration result)
                         (if (zero? iteration)
                             (set! first result)
                             (unless (equal? result first)
                               (error 'bmbench-improved
                                      "sequential result changed (size ~a)" len)))))))

      (for ([w (in-list worker-counts)])
        (define params
          (append (base-params len repeats)
                  (list (list 'workers w))))
        (run-benchmark
         (λ () (vector-boyer-moore-majority/parallel/improved
                vec
                #:workers w
                #:equal equal/heavy
                #:threshold threshold
                #:chunk-size chunk-size
                #:chunk-multiplier chunk-multiplier))
         #:name 'bmbench-improved
         #:variant 'parallel-improved
         #:repeat repeats
         #:log-writer writer
         #:params params
         #:metadata metadata
         #:check (λ (_ result)
                   (unless (equal? result sequential-result)
                     (error 'bmbench-improved
                            "parallel result mismatch (size ~a workers ~a)"
                            len w))))))

    (close-log-writer writer)))
