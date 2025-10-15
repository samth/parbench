#lang racket

(require rackunit
         "../../benchmarks/nas/is.rkt")

(define small-keys 1024)

(test-case "Sequential IS benchmark runs"
  (define result (is #:class 'S #:total-keys small-keys #:workers 1))
  (check-true (is-result? result))
  (check-equal? (vector-length (is-result-keys result)) small-keys))

(test-case "Parallel IS benchmark runs"
  (define result (is #:class 'S #:total-keys small-keys #:workers 4))
  (check-true (is-result? result))
  (check-equal? (vector-length (is-result-keys result)) small-keys))

(test-case "Sequential vs parallel agreement"
  (define seq-result (is #:class 'S #:total-keys small-keys #:workers 1))
  (define par-result (is #:class 'S #:total-keys small-keys #:workers 4))
  (check-equal? (is-result-keys seq-result) (is-result-keys par-result)))

(test-case "Result is sorted"
  (define result (is #:class 'S #:total-keys small-keys #:workers 1))
  (define keys (is-result-keys result))
  (for ([i (in-range 1 (vector-length keys))])
    (check-true (<= (vector-ref keys (- i 1))
                    (vector-ref keys i)))))

(test-case "Stats structure is correct"
  (define result (is #:class 'S #:total-keys small-keys #:workers 1))
  (define stats (is-result-stats result))
  (check-true (is-stats? stats))
  (check-equal? (is-stats-num-keys stats) small-keys))
