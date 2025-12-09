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

;; -------- Boyer-Moore (vector-only, multiple values) --------

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

;; -------- Sequential baseline --------

(define (vector-boyer-moore-majority/sequential v #:equal [equal equal?])
  (define-values (c n) (bm-scan-vec v 0 (vector-length v) equal))
  (bm-verify-vec v c equal))

;; -------- Parallel scan + parallel tree-merge (thread #:pool) --------

(define (vector-boyer-moore-majority/parallel v
          #:workers [workers (processor-count)]
          #:equal   [equal   equal?])
  (unless (vector? v) (error 'vector-boyer-moore-majority/parallel "vector required"))
  (define n (vector-length v))
  (cond
    [(zero? n) #f]
    [else
     (define k (max 1 (min workers n)))
     (define p (make-parallel-thread-pool k))

     ;; leaf scans
     (define scans
       (for/list ([rg (in-list (chunk-ranges n k))])
         (define start (car rg)) (define end (cdr rg))
         (thread (lambda () (bm-scan-vec v start end equal))
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
                 (thread (lambda ()
                           (define-values (c1 n1) (thread-wait t1))
                           (define-values (c2 n2) (thread-wait t2))
                           (bm-merge c1 n1 c2 n2 equal))
                         #:pool p #:keep 'results)]
                [(list t1) t1])))
          (tree-merge next-level)]))

     (define-values (cand cnt) (tree-merge scans))
     (parallel-thread-pool-close p)
     (bm-verify-vec v cand equal)]))

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
  (define n 1000000)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define skip-sequential #f)
  (define majority-value 7)
  (define majority-probability 0.6)
  (define majority-kinds 64)
  (define rng-seed 424242)

  (void
   (command-line
   #:program "bmbench.rkt"
   #:once-each
   [("--n") arg "Vector length"
    (set! n (parse-positive-integer arg 'bmbench))]
   [("--workers") arg "Parallel thread count"
    (set! workers (parse-positive-integer arg 'bmbench))]
   [("--repeat") arg "Benchmark repetitions"
    (set! repeat (parse-positive-integer arg 'bmbench))]
   [("--log") arg "Optional S-expression log path"
    (set! log-path arg)]
   [("--skip-sequential") "Skip sequential baseline"
    (set! skip-sequential #t)]
   [("--majority") arg "Majority value inserted into generated vectors"
    (set! majority-value (parse-integer arg 'bmbench))]
   [("--probability") arg "Majority probability between 0 and 1"
    (set! majority-probability (parse-probability arg 'bmbench))]
   [("--kinds") arg "Distinct value kinds allocated to non-majority elements"
    (set! majority-kinds (parse-positive-integer arg 'bmbench))]
   [("--seed") arg "Random seed integer, or 'none' to leave RNG state untouched"
    (set! rng-seed (maybe-parse-seed arg 'bmbench))]))

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
                       (list 'workers workers)))

  (define sequential-result #f)

  (unless skip-sequential
    (set! sequential-result
          (run-benchmark
           (lambda () (vector-boyer-moore-majority/sequential vec #:equal equal/heavy))
           #:name 'bmbench
           #:variant 'sequential
           #:repeat repeat
           #:log-writer writer
           #:params params
           #:metadata metadata)))

  (if sequential-result
      (run-benchmark
       (lambda () (vector-boyer-moore-majority/parallel vec
                                                        #:workers workers
                                                        #:equal equal/heavy))
       #:name 'bmbench
       #:variant 'parallel
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata
       #:check (lambda (_ result)
                 (unless (equal? result sequential-result)
                   (error 'bmbench "parallel result mismatch"))))
      (run-benchmark
       (lambda () (vector-boyer-moore-majority/parallel vec
                                                        #:workers workers
                                                        #:equal equal/heavy))
       #:name 'bmbench
       #:variant 'parallel
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata))

  (close-log-writer writer))
