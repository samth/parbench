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
    (check-equal? (hash-ref idx "c") '((1 . 1))))

  ;; ======== Query Tests ========

  (test-case "query generation"
    (random-seed 42)
    (define docs (generate-documents 1000))
    (define idx (inverted-index-sequential docs))
    (define queries (generate-queries idx 10 4))
    (check-equal? (vector-length queries) 10)
    ;; Each query should have 2-4 words
    (for ([q (in-vector queries)])
      (check-true (>= (vector-length q) 2))
      (check-true (<= (vector-length q) 5))))

  (test-case "sequential equals parallel queries"
    (random-seed 42)
    (define docs (generate-documents 10000))
    (define idx (inverted-index-sequential docs))
    (define queries (generate-queries idx 100 3))
    (define seq (search-sequential idx queries))
    (define par (search-parallel idx queries #:workers 4))
    (check-equal? seq par))

  (test-case "query with known results"
    ;; Manual test with known documents
    (define docs #(#("hello" "world") #("hello" "foo") #("bar" "world")))
    (define idx (inverted-index-sequential docs))
    ;; Query for "hello" AND "world" -> only doc 0
    (define queries #(#("hello" "world")))
    (check-equal? (search-sequential idx queries) 1)
    (check-equal? (search-parallel idx queries #:workers 2) 1))

  (test-case "empty query result"
    (define docs #(#("a" "b") #("c" "d")))
    (define idx (inverted-index-sequential docs))
    ;; Query for words that don't co-occur
    (define queries #(#("a" "c")))  ; "a" in doc 0, "c" in doc 1 -> no intersection
    (check-equal? (search-sequential idx queries) 0)
    (check-equal? (search-parallel idx queries #:workers 2) 0)))
