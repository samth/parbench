#lang racket

(require rackunit
         "../benchmarks/racket/bmbench.rkt")

(module+ test
  (define (both vec #:equal [eq equal?] #:workers [workers 4])
    (values (vector-boyer-moore-majority/sequential vec #:equal eq)
            (vector-boyer-moore-majority/parallel vec
                                                  #:workers workers
                                                  #:equal eq)))

  (test-case "odd-length vector with clear majority"
    (define vec #(1 1 1 2 3 1 4 1 5))
    (define-values (seq par) (both vec))
    (check-equal? seq 1)
    (check-equal? par 1))

  (test-case "even-length vector without majority"
    (define vec #(1 1 2 2 3 3))
    (define-values (seq par) (both vec))
    (check-false seq)
    (check-false par))

  (test-case "single element vector"
    (define vec #(42))
    (define-values (seq par) (both vec #:workers 1))
    (check-equal? seq 42)
    (check-equal? par 42))

  (test-case "empty vector returns #f"
    (define vec #())
    (define-values (seq par) (both vec))
    (check-false seq)
    (check-false par))

  (test-case "custom equality with case-insensitive strings"
    (define vec #("A" "b" "a" "A" "c" "a" "A"))
    (define-values (seq par)
      (both vec #:equal string-ci=? #:workers 3))
    (check-true (string-ci=? seq "a"))
    (check-true (string-ci=? par "a")))

  (test-case "parallel result matches sequential on randomized data"
    (random-seed 12345)
    (define vec (make-majority-vector 200 7 #:p 0.55 #:kinds 16))
    (define-values (seq par) (both vec #:workers 8))
    (check-equal? par seq)
    (check-equal? seq 7))

  (test-case "parallel handles worker counts greater than vector length"
    (define vec #(2 2 2 3 4))
    (define-values (_ par) (both vec #:workers 32))
    (check-equal? par 2))

  (test-case "no majority with heavy equality comparator"
    (define vec (make-majority-vector 50 5 #:p 0.5 #:kinds 32))
    (define-values (seq par)
      (values (vector-boyer-moore-majority/sequential vec #:equal equal/heavy)
              (vector-boyer-moore-majority/parallel vec #:equal equal/heavy)))
    (check-false seq)
    (check-false par))

  (test-case "parallel requires vectors"
    (check-exn exn:fail?
      (λ () (vector-boyer-moore-majority/parallel '(1 2 3)))))

  (test-case "sequential agrees with majority when candidate appears late"
    (define vec #(9 8 7 7 7 7 7))
    (define-values (seq par) (both vec))
    (check-equal? seq 7)
    (check-equal? par 7)))
