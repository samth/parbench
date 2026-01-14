#lang racket

(require rackunit
         "../benchmarks/racket/inverted-index-opt.rkt")

(module+ test
  (test-case "optimistic hash basic operations"
    (define ht (make-opt-hash))
    (opt-hash-set! ht 'a 1)
    (opt-hash-set! ht 'b 2)
    (check-equal? (opt-hash-ref ht 'a) 1)
    (check-equal? (opt-hash-ref ht 'b) 2)
    (check-equal? (opt-hash-ref ht 'c 'default) 'default))

  (test-case "build sequential index"
    (random-seed 42)
    (define docs (generate-documents 100))
    (define idx (build-opt-index-sequential docs))
    ;; Should have some entries
    (check-true (> (length (opt-hash-keys-flat idx)) 0)))

  (test-case "build parallel equals sequential"
    (random-seed 42)
    (define docs (generate-documents 1000))
    (define seq-idx (build-opt-index-sequential docs))
    (random-seed 42)
    (define docs2 (generate-documents 1000))
    (define par-idx (build-opt-index-parallel docs2 #:workers 2))
    ;; Both should produce indexes with same word count
    (check-equal? (length (opt-hash-keys-flat seq-idx))
                  (length (opt-hash-keys-flat par-idx))))

  (test-case "query sequential equals parallel"
    (random-seed 42)
    (define docs (generate-documents 1000))
    (define idx (build-opt-index-sequential docs))
    (define queries (generate-queries-opt idx 50 3))
    (define seq-result (search-sequential-opt idx queries))
    (define par-result (search-parallel-opt idx queries #:workers 2))
    (check-equal? seq-result par-result))

  (test-case "query with known results"
    ;; Build index manually with symbols
    (define docs #(#(hello world) #(hello foo) #(bar world)))
    (define idx (make-opt-hash 16))
    ;; Add postings manually
    (opt-hash-set! idx 'hello '((0 . 1) (1 . 1)))
    (opt-hash-set! idx 'world '((0 . 1) (2 . 1)))
    (opt-hash-set! idx 'foo '((1 . 1)))
    (opt-hash-set! idx 'bar '((2 . 1)))
    ;; Query for hello AND world -> only doc 0
    (define queries #(#(hello world)))
    (check-equal? (search-sequential-opt idx queries) 1)
    (check-equal? (search-parallel-opt idx queries #:workers 2) 1)))
