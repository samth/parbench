#lang racket

(require rackunit
         racket/match
         racket/set
         "../../benchmarks/mpl/bfs.rkt")

(define edges '((0 1) (1 2) (1 3) (2 4) (3 4)))
(define vertex-count 5)

(define (build-adjacency n edge-list)
  (define adj (make-vector n '()))
  (for ([edge edge-list])
    (match-define (list u v) edge)
    (vector-set! adj u (cons v (vector-ref adj u)))
    (vector-set! adj v (cons u (vector-ref adj v))))
  adj)

(define adjacency (build-adjacency vertex-count edges))

(define (parent->dist parent source)
  (define n (vector-length parent))
  (define dist (make-vector n -1))
  (for ([v (in-range n)])
    (define p (vector-ref parent v))
    (cond
      [(= v source)
       (when (= p source)
         (vector-set! dist v 0))]
      [(= p -1) (void)]
      [else
       (define maybe-distance
         (let loop ([current v] [steps 0] [seen (seteq)])
           (cond
             [(= current source) steps]
             [(or (< current 0) (>= current n)) #f]
             [(set-member? seen current) #f]
             [else
              (let ([next (vector-ref parent current)])
                (loop next (add1 steps) (set-add seen current)))])))
       (when maybe-distance
         (vector-set! dist v maybe-distance))]))
  dist)

(define (valid-parent? parent)
  (define n (vector-length parent))
  (for/and ([v (in-range n)])
    (define p (vector-ref parent v))
    (cond
      [(= p -1) #t]
      [(= p v) #t]
      [else (ormap (λ (nbr) (= nbr p))
                   (vector-ref adjacency v))])))

(module+ test
  (define g (make-graph vertex-count edges))
  (define source 0)
  (define seq-parent (bfs-sequential g source))
  (define par-parent (bfs-parallel g source 3))

  (check-true (valid-parent? seq-parent))
  (check-true (valid-parent? par-parent))

  (check-equal? (parent->dist seq-parent source)
                (parent->dist par-parent source)))
