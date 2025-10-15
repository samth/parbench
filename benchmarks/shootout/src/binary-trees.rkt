#lang racket

(struct tree (left right) #:transparent)

(define (make-tree depth)
  (if (zero? depth)
      (tree #f #f)
      (tree (make-tree (sub1 depth))
            (make-tree (sub1 depth)))))

(define (check-tree t)
  (if (tree-left t)
      (+ 1
         (check-tree (tree-left t))
         (check-tree (tree-right t)))
      1))

(module+ main
  (define args (current-command-line-arguments))
  (define n (if (> (vector-length args) 0)
                (string->number (vector-ref args 0))
                16))
  (define min-depth 4)
  (define max-depth (max (add1 min-depth) n))
  (define stretch-depth (add1 max-depth))

  (printf "stretch tree of depth ~a\t check: ~a\n"
          stretch-depth
          (check-tree (make-tree stretch-depth)))

  (define long-lived-tree (make-tree max-depth))

  (for ([depth (in-range min-depth (add1 max-depth) 2)])
    (define iterations (arithmetic-shift 1 (- (+ max-depth min-depth) depth)))
    (define check-sum
      (for/sum ([i (in-range iterations)])
        (check-tree (make-tree depth))))
    (printf "~a\t trees of depth ~a\t check: ~a\n"
            iterations depth check-sum))

  (printf "long lived tree of depth ~a\t check: ~a\n"
          max-depth
          (check-tree long-lived-tree)))
