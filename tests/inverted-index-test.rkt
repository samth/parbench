#lang racket

(require rackunit
         "../benchmarks/racket/inverted-index.rkt")

(module+ test
  (test-case "basic inverted index - small"
    (random-seed 42)
    (define docs (generate-documents 100))  ; ~10 docs, ~10 words each
    (define seq (inverted-index-sequential docs))
    (define par (inverted-index-parallel docs #:workers 2))
    (check-true (verify-indexes seq par)))

  (test-case "sequential equals parallel - medium"
    (random-seed 123)
    (define docs (generate-documents 10000))  ; ~100 docs, ~100 words each
    (define seq (inverted-index-sequential docs))
    (define par (inverted-index-parallel docs #:workers 4))
    (check-true (verify-indexes seq par)))

  (test-case "empty documents"
    (define docs #())
    (define seq (inverted-index-sequential docs))
    (define par (inverted-index-parallel docs #:workers 2))
    (check-equal? (hash-count seq) 0)
    (check-equal? (hash-count par) 0))

  (test-case "single document"
    (random-seed 99)
    (define docs (generate-documents 1))
    (define seq (inverted-index-sequential docs))
    (define par (inverted-index-parallel docs #:workers 2))
    (check-true (verify-indexes seq par)))

  (test-case "more workers than documents"
    (random-seed 42)
    (define docs (generate-documents 4))  ; ~2 docs
    (define seq (inverted-index-sequential docs))
    (define par (inverted-index-parallel docs #:workers 16))
    (check-true (verify-indexes seq par)))

  (test-case "index structure is correct"
    ;; Manual test with known input
    (define docs #(#("a" "b" "a") #("b" "c" "b")))
    (define idx (inverted-index-sequential docs))
    ;; word "a" appears in doc 0 with count 2
    (check-equal? (hash-ref idx "a") '((0 . 2)))
    ;; word "b" appears in doc 0 (count 1) and doc 1 (count 2)
    (define b-postings (sort (hash-ref idx "b") < #:key car))
    (check-equal? b-postings '((0 . 1) (1 . 2)))
    ;; word "c" appears in doc 1 with count 1
    (check-equal? (hash-ref idx "c") '((1 . 1)))))
