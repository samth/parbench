#lang racket

(require rackunit
         racket/list
         racket/match
         "../../benchmarks/mpl/msf.rkt")

(define edges
  '((0 1 1)
    (1 2 2)
    (2 3 3)
    (0 3 4)
    (0 2 5)))

(define (build-graph n edge-list)
  (define adj (make-vector n '()))
  (for ([edge edge-list])
    (match-define (list u v w) edge)
    (vector-set! adj u (cons (cons v w) (vector-ref adj u)))
    (vector-set! adj v (cons (cons u w) (vector-ref adj v))))
  (for/vector ([neighbors (in-vector adj)])
    (list->vector neighbors)))

(define test-graph (build-graph 4 edges))

(define (canon edge)
  (match-define (list u v w) edge)
  (list (min u v) (max u v) w))

(define (normalize edge-list)
  (sort (map canon edge-list)
        (λ (a b)
          (or (< (first a) (first b))
              (and (= (first a) (first b))
                   (or (< (second a) (second b))
                       (and (= (second a) (second b))
                            (< (third a) (third b)))))))))

(module+ test
  (define seq (msf-sequential test-graph))
  (define par (msf-parallel test-graph 3))
  (check-true (verify-spanning-forest test-graph seq))
  (check-true (verify-spanning-forest test-graph par))
  (check-equal? (normalize seq) (normalize par)))
