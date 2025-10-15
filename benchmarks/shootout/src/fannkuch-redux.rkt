#lang racket

(define (fannkuch n)
  (define perm (for/vector ([i (in-range n)]) i))
  (define perm1 (for/vector ([i (in-range n)]) i))
  (define count (make-vector n 0))
  (define flips-max 0)
  (define checksum 0)

  (define (next-permutation)
    (define first (vector-ref perm1 0))
    (for ([i (in-range n 1 -1)])
      (vector-set! perm1 (- i 1) (vector-ref perm1 i)))
    (vector-set! perm1 (sub1 n) first))

  (define (permute)
    (let loop ([r n])
      (when (> r 1)
        (vector-set! count (sub1 r) r)
        (loop (sub1 r)))))

  (permute)

  (let loop ()
    (vector-copy! perm 0 perm1)
    (let flipping ([flips 0])
      (define k (vector-ref perm 0))
      (when (> k 0)
        (for ([i (in-range (add1 (quotient (+ k 1) 2)))])
          (define tmp (vector-ref perm i))
          (vector-set! perm i (vector-ref perm (- k i)))
          (vector-set! perm (- k i) tmp))
        (flipping (add1 flips)))
      (set! flips-max (max flips-max flips))
      (set! checksum (+ checksum (* (if (even? (vector-ref perm1 0)) 1 -1) flips))))
    (let loop2 ([r 1])
      (when (< r n)
        (define c (vector-ref count r))
        (if (> c 1)
            (begin
              (vector-set! count r (sub1 c))
              (let ([first (vector-ref perm1 0)])
                (for ([i (in-range r)])
                  (vector-set! perm1 i (vector-ref perm1 (add1 i))))
                (vector-set! perm1 r first))
              (loop))
            (begin
              (vector-set! count r r)
              (loop2 (add1 r))))))))

  (values checksum flips-max))

(module+ main
  (define args (current-command-line-arguments))
  (define n (if (> (vector-length args) 0)
                (string->number (vector-ref args 0))
                12))
  (define-values (checksum flips) (fannkuch n))
  (printf "~a\nPfannkuchen(~a) = ~a\n" checksum n flips))
