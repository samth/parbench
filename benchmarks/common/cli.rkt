#lang racket

(require racket/string)

(provide parse-positive-integer
         parse-integer
         parse-positive-list
         parse-probability
         maybe-parse-seed
         ensure-non-empty)

(define (parse-positive-integer str label)
  (define n (string->number str))
  (unless (and n (integer? n) (> n 0))
    (error label "expected positive integer, got ~a" str))
  (inexact->exact n))

(define (parse-integer str label)
  (define n (string->number str))
  (unless (and n (integer? n))
    (error label "expected integer, got ~a" str))
  (inexact->exact n))

(define (parse-positive-list str label)
  (define items
    (for/list ([piece (in-list (string-split str "," #:trim? #f))]
               #:unless (string=? "" (string-trim piece)))
      (string-trim piece)))
  (ensure-non-empty items label)
  (for/list ([piece (in-list items)])
    (parse-positive-integer piece label)))

(define (parse-probability str label)
  (define n (string->number str))
  (unless (and n (real? n) (> n 0) (<= n 1))
    (error label "expected probability in (0,1], got ~a" str))
  (exact->inexact n))

(define (maybe-parse-seed str label)
  (define trimmed (string-downcase (string-trim str)))
  (cond
    [(member trimmed '("none" "null" "")) #f]
    [else (parse-integer str label)]))

(define (ensure-non-empty items label)
  (unless (pair? items)
    (error label "expected at least one value"))
  items)
