#lang racket

(require rackunit
         racket/list
         "../../benchmarks/mpl/convex-hull.rkt")

(define sample-points
  (list (cons 0.0 0.0)
        (cons 1.0 0.0)
        (cons 1.0 1.0)
        (cons 0.0 1.0)
        (cons 0.5 0.5)
        (cons 0.2 0.8)))

(define (canonical hull)
  (sort (map (λ (p) (list (car p) (cdr p))) hull)
        (λ (a b)
          (or (< (first a) (first b))
              (and (= (first a) (first b))
                   (< (second a) (second b)))))))

(module+ test
  (define seq (convex-hull-sequential sample-points))
  (define par (convex-hull-parallel sample-points 3 1))
  (check-true (verify-convex-hull sample-points seq))
  (check-true (verify-convex-hull sample-points par))
  (check-equal? (canonical seq) (canonical par)))
