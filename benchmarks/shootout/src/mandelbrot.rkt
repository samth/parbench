#lang racket

(define (mandelbrot width)
  (define height width)
  (define max-iter 50)
  (define result (make-bytes (/ (* width height) 8)))
  (for ([y (in-range height)])
    (define Ci (- (* 2.0 y (/ 1 width)) 1.0))
    (for ([x (in-range 0 width 8)])
      (define byte 0)
      (for ([i (in-range 8)])
        (define Cr (- (* 2.0 (+ x i) (/ 1 width)) 1.5))
        (define zr 0.0)
        (define zi 0.0)
        (define escape #t)
        (let loop ([iter max-iter]
                   [zr zr]
                   [zi zi])
          (when (> iter 0)
            (define zr2 (- (* zr zr) (* zi zi) Cr))
            (define zi2 (+ (* 2 zr zi) Ci))
            (if (> (+ (* zr2 zr2) (* zi2 zi2)) 4)
                (set! escape #f)
                (loop (sub1 iter) zr2 zi2))))
        (when escape
          (set! byte (bitwise-ior byte (arithmetic-shift 1 (- 7 i))))))
      (bytes-set! result (/ (+ (* y width) x) 8) byte)))
  result)

(module+ main
  (define args (current-command-line-arguments))
  (define n (if (> (vector-length args) 0)
                (string->number (vector-ref args 0))
                200))
  (printf "P4\n~a ~a\n" n n)
  (display (mandelbrot n)))
