#lang racket

(define (A i j)
  (define ij (+ i j))
  (/ 1.0 (+ (* 0.5 ij (+ ij 1))
            (+ i 1))))

(define (A-times-u u)
  (define n (vector-length u))
  (for/vector #:length n ([i (in-range n)])
    (for/sum ([j (in-range n)])
      (* (vector-ref u j) (A i j)))))

(define (At-times-u u)
  (define n (vector-length u))
  (for/vector #:length n ([i (in-range n)])
    (for/sum ([j (in-range n)])
      (* (vector-ref u j) (A j i)))))

(define (AtA-times-u u)
  (At-times-u (A-times-u u)))

(define (spectral-norm n iter)
  (let loop ([u (make-vector n 1.0)]
             [count iter])
    (define v (AtA-times-u u))
    (if (zero? count)
        (let ([vBv (for/sum ([i (in-range n)]) (* (vector-ref u i) (vector-ref v i)))]
              [vv (for/sum ([i (in-range n)]) (* (vector-ref v i) (vector-ref v i)))])
          (sqrt (/ vBv vv)))
        (loop v (sub1 count)))))

(module+ main
  (define n (let ([args (current-command-line-arguments)])
              (if (> (vector-length args) 0)
                  (string->number (vector-ref args 0))
                  1000)))
  (printf "~.9f\n" (spectral-norm n 10)))
