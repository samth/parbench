#lang racket
(require racket/future         ; processor-count
         racket/match)

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
  (define v (make-majority-vector N 7 #:p 0.6 #:kinds 64))

  (define (bench label thunk)
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (displayln label)
    (time (thunk)))

  (bench "sequential"
         (λ () (vector-boyer-moore-majority/sequential v #:equal equal/heavy)))

  ;; reuse pool to avoid setup noise
  (for ([w '(1 2 4 8)])
    (define p (make-parallel-thread-pool w))
    (bench (format "parallel ~a worker(s)" w)
           (λ () (vector-boyer-moore-majority/parallel v #:pool p #:equal equal/heavy)))
    (parallel-thread-pool-close p)))


