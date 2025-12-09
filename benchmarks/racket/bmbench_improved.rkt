#lang racket

(require racket/place
         racket/match
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide vector-boyer-moore-majority/sequential
         vector-boyer-moore-majority/parallel
         make-majority-vector
         equal/heavy)

;; -------- Boyer-Moore Helpers --------

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

(define (vector-boyer-moore-majority/parallel v
          #:workers [workers (processor-count)]
          #:equal [equal equal?]
          #:threshold [threshold 50000]
          #:chunk-size [chunk-size #f]
          #:chunk-multiplier [chunk-multiplier 2])
  (unless (vector? v)
    (error 'vector-boyer-moore-majority/parallel "vector required"))
  (define n (vector-length v))
  (cond
    [(zero? n) #f]
    [(<= n threshold)
     (vector-boyer-moore-majority/sequential v #:equal equal)]
    [else
     (define chunk-count (compute-chunk-count n workers chunk-size chunk-multiplier threshold))
     (define ranges (chunk-ranges n chunk-count))
     (define pool (make-parallel-thread-pool (min workers chunk-count)))

     (define results
       (for/list ([rg (in-list ranges)])
         (define start (car rg))
         (define end (cdr rg))
         (thread (lambda () (bm-scan-vec v start end equal))
                 #:pool pool #:keep 'results)))

     (define-values (cand cnt)
       (let loop ([threads results] [cand #f] [cnt 0])
         (cond
           [(null? threads) (values cand cnt)]
           [else
            (define-values (c n) (thread-wait (car threads)))
            (define-values (next-c next-n)
              (if cand
                  (bm-merge cand cnt c n equal)
                  (values c n)))
            (loop (cdr threads) next-c next-n)])))

     (parallel-thread-pool-close pool)
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
  (define n 1000000)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define skip-sequential #f)
  (define majority-value 7)
  (define majority-probability 0.6)
  (define majority-kinds 64)
  (define rng-seed 424242)
  (define threshold 50000)
  (define chunk-size #f)
  (define chunk-multiplier 2)

  (void
   (command-line
   #:program "bmbench_improved.rkt"
   #:once-each
   [("--n") arg "Vector length"
    (set! n (parse-positive-integer arg 'bmbench-improved))]
   [("--workers") arg "Parallel thread count"
    (set! workers (parse-positive-integer arg 'bmbench-improved))]
   [("--repeat") arg "Benchmark repetitions"
    (set! repeat (parse-positive-integer arg 'bmbench-improved))]
   [("--log") arg "Optional S-expression log path"
    (set! log-path arg)]
   [("--skip-sequential") "Skip sequential baseline"
    (set! skip-sequential #t)]
   [("--majority") arg "Majority value inserted into generated vectors"
    (set! majority-value (parse-integer arg 'bmbench-improved))]
   [("--probability") arg "Majority probability between 0 and 1"
    (set! majority-probability (parse-probability arg 'bmbench-improved))]
   [("--kinds") arg "Distinct kinds allocated to non-majority elements"
    (set! majority-kinds (parse-positive-integer arg 'bmbench-improved))]
   [("--seed") arg "Random seed integer, or 'none' to leave RNG state untouched"
    (set! rng-seed (maybe-parse-seed arg 'bmbench-improved))]
   [("--threshold") arg "Vector length threshold below which runs stay sequential"
    (set! threshold (parse-positive-integer arg 'bmbench-improved))]
   [("--chunk-size") arg "Preferred chunk size per worker (default auto)"
    (set! chunk-size (if (or (string-ci=? arg "auto") (string-ci=? arg "none"))
                         #f
                         (parse-positive-integer arg 'bmbench-improved)))]
   [("--chunk-multiplier") arg "Upper bound multiplier for chunks relative to workers"
    (set! chunk-multiplier (parse-positive-integer arg 'bmbench-improved))]))

  (when rng-seed (random-seed rng-seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define vec (make-majority-vector n majority-value
                                    #:p majority-probability
                                    #:kinds majority-kinds))
  (define params (list (list 'n n)
                       (list 'majority majority-value)
                       (list 'probability majority-probability)
                       (list 'kinds majority-kinds)
                       (list 'threshold threshold)
                       (list 'chunk-size (or chunk-size 'auto))
                       (list 'chunk-multiplier chunk-multiplier)
                       (list 'workers workers)))

  (define sequential-result #f)

  (unless skip-sequential
    (set! sequential-result
          (run-benchmark
           (lambda () (vector-boyer-moore-majority/sequential vec #:equal equal/heavy))
           #:name 'bmbench-improved
           #:variant 'sequential
           #:repeat repeat
           #:log-writer writer
           #:params params
           #:metadata metadata)))

  (if sequential-result
      (run-benchmark
       (lambda () (vector-boyer-moore-majority/parallel
                   vec
                   #:workers workers
                   #:equal equal/heavy
                   #:threshold threshold
                   #:chunk-size chunk-size
                   #:chunk-multiplier chunk-multiplier))
       #:name 'bmbench-improved
       #:variant 'parallel
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata
       #:check (lambda (_ result)
                 (unless (equal? result sequential-result)
                   (error 'bmbench-improved "parallel result mismatch"))))
      (run-benchmark
       (lambda () (vector-boyer-moore-majority/parallel
                   vec
                   #:workers workers
                   #:equal equal/heavy
                   #:threshold threshold
                   #:chunk-size chunk-size
                   #:chunk-multiplier chunk-multiplier))
       #:name 'bmbench-improved
       #:variant 'parallel
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata))

  (close-log-writer writer))
