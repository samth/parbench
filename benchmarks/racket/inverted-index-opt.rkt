#lang racket

;; Inverted Index Benchmark with Optimistic Hash Table
;;
;; Compares query performance using:
;; 1. Standard Racket hash table (poor scaling due to semaphore)
;; 2. Optimistic hash table (better scaling for reads)
;;
;; The optimistic hash uses eq? keys, so we intern words as symbols.

(require racket/place
         racket/match
         racket/set
         racket/fixnum
         (only-in '#%unsafe
                  unsafe-box*-cas!
                  unsafe-unbox*
                  unsafe-set-box*!
                  unsafe-fx+
                  unsafe-fx=
                  unsafe-fxmodulo
                  unsafe-vector-ref
                  unsafe-vector-set!)
         (only-in '#%kernel
                  memory-order-acquire
                  memory-order-release)
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide make-opt-hash
         opt-hash-ref
         opt-hash-set!
         opt-hash-keys-flat
         generate-documents
         build-opt-index-sequential
         build-opt-index-parallel
         generate-queries-opt
         search-sequential-opt
         search-parallel-opt)

;; ============================================================
;; Optimistic Hash Table Implementation (from parallel-hash)
;; Uses eq? comparison - keys must be symbols or eq?-comparable
;; ============================================================

(struct entry (key value next) #:mutable)

(struct opt-hash
  (version       ; box<fixnum> - incremented on layout changes
   lock          ; box<#f|#t> - exclusive lock for writes
   buckets       ; box<vector of box<entry>>
   count-box)
  #:mutable)

(define (make-opt-hash [size 16])
  (define buckets (make-vector size #f))
  (for ([i (in-range size)])
    (vector-set! buckets i (box #f)))
  (opt-hash (box 0) (box #f) (box buckets) (box 0)))

(define (opt-hash-ref ht key [default #f])
  (let loop ()
    (define v1 (unsafe-unbox* (opt-hash-version ht)))
    (memory-order-acquire)
    (define buckets (unsafe-unbox* (opt-hash-buckets ht)))
    (define idx (unsafe-fxmodulo (eq-hash-code key) (vector-length buckets)))
    (define result
      (let chain-loop ([e (unsafe-unbox* (unsafe-vector-ref buckets idx))])
        (cond
          [(not e) default]
          [(eq? (entry-key e) key) (entry-value e)]
          [else (chain-loop (entry-next e))])))
    (memory-order-acquire)
    (define v2 (unsafe-unbox* (opt-hash-version ht)))
    (if (unsafe-fx= v1 v2) result (loop))))

(define (opt-hash-set! ht key value)
  (let lock-loop ()
    (if (unsafe-box*-cas! (opt-hash-lock ht) #f #t)
        (memory-order-acquire)
        (lock-loop)))
  (define buckets (unsafe-unbox* (opt-hash-buckets ht)))
  (define idx (unsafe-fxmodulo (eq-hash-code key) (vector-length buckets)))
  (define bucket-box (unsafe-vector-ref buckets idx))
  (let chain-loop ([e (unsafe-unbox* bucket-box)])
    (cond
      [(not e)
       (define head (unsafe-unbox* bucket-box))
       (unsafe-set-box*! bucket-box (entry key value head))
       (let ([c (unsafe-unbox* (opt-hash-count-box ht))])
         (unsafe-set-box*! (opt-hash-count-box ht) (unsafe-fx+ c 1)))
       (when (> (unsafe-unbox* (opt-hash-count-box ht))
                (* (vector-length buckets) 0.75))
         (opt-resize! ht))]
      [(eq? (entry-key e) key)
       (set-entry-value! e value)]
      [else (chain-loop (entry-next e))]))
  (memory-order-release)
  (unsafe-set-box*! (opt-hash-lock ht) #f))

(define (opt-resize! ht)
  (let ([v (unsafe-unbox* (opt-hash-version ht))])
    (unsafe-set-box*! (opt-hash-version ht) (unsafe-fx+ v 1)))
  (memory-order-release)
  (define old-buckets (unsafe-unbox* (opt-hash-buckets ht)))
  (define new-size (* (vector-length old-buckets) 2))
  (define new-buckets (make-vector new-size #f))
  (for ([i (in-range new-size)])
    (vector-set! new-buckets i (box #f)))
  (for ([i (in-range (vector-length old-buckets))])
    (let loop ([e (unsafe-unbox* (unsafe-vector-ref old-buckets i))])
      (when e
        (define k (entry-key e))
        (define idx (unsafe-fxmodulo (eq-hash-code k) new-size))
        (define bucket-box (unsafe-vector-ref new-buckets idx))
        (unsafe-set-box*! bucket-box (entry k (entry-value e) (unsafe-unbox* bucket-box)))
        (loop (entry-next e)))))
  (unsafe-set-box*! (opt-hash-buckets ht) new-buckets)
  (memory-order-release)
  (let ([v (unsafe-unbox* (opt-hash-version ht))])
    (unsafe-set-box*! (opt-hash-version ht) (unsafe-fx+ v 1))))

(define (opt-hash-keys ht)
  (define buckets (unsafe-unbox* (opt-hash-buckets ht)))
  (for*/list ([i (in-range (vector-length buckets))]
              [e (in-value (unsafe-unbox* (unsafe-vector-ref buckets i)))]
              #:when e
              [entry (in-value e)])
    (let loop ([e entry] [acc '()])
      (if e
          (loop (entry-next e) (cons (entry-key e) acc))
          acc))))

(define (opt-hash-keys-flat ht)
  (apply append (opt-hash-keys ht)))

;; ============================================================
;; Document Generation (using symbols for eq? comparison)
;; ============================================================

(define (zipf-sample/simple vocab-size)
  (define u (+ 0.0001 (random)))
  (define rank (inexact->exact (floor (/ 1.0 u))))
  (modulo rank vocab-size))

(define (generate-documents n)
  ;; Generate documents using interned symbols for words
  (define num-docs (max 1 (integer-sqrt n)))
  (define doc-size (max 1 (integer-sqrt n)))
  (define vocab-size (max 1000 (quotient n 1000)))
  ;; Use symbols so eq? works in optimistic hash
  (define vocab (for/vector ([i vocab-size]) (string->symbol (format "word~a" i))))
  (for/vector ([doc-id (in-range num-docs)])
    (for/vector ([_ (in-range doc-size)])
      (vector-ref vocab (zipf-sample/simple vocab-size)))))

;; ============================================================
;; Build Index (into optimistic hash)
;; ============================================================

(define (build-opt-index docs)
  ;; Build inverted index using optimistic hash
  (define vocab-size (max 1000 (quotient (vector-length docs) 10)))
  (define index (make-opt-hash (* vocab-size 2)))
  (for ([doc (in-vector docs)]
        [doc-id (in-naturals)])
    (define word-counts (make-hasheq))
    (for ([word (in-vector doc)])
      (hash-update! word-counts word add1 0))
    (for ([(word count) (in-hash word-counts)])
      (define existing (opt-hash-ref index word '()))
      (opt-hash-set! index word (cons (cons doc-id count) existing))))
  index)

;; ============================================================
;; Query Processing
;; ============================================================

(define (process-query-opt index query)
  ;; Query using optimistic hash
  (define posting-lists
    (for/list ([word (in-vector query)])
      (opt-hash-ref index word '())))
  (cond
    [(null? posting-lists) (set)]
    [(ormap null? posting-lists) (set)]
    [else
     (define first-docs (list->set (map car (car posting-lists))))
     (for/fold ([result first-docs])
               ([postings (in-list (cdr posting-lists))])
       (set-intersect result (list->set (map car postings))))]))

(define (search-sequential-opt index queries)
  (for/sum ([query (in-vector queries)])
    (set-count (process-query-opt index query))))

(define (search-parallel-opt index queries #:workers [workers (processor-count)])
  (define n (vector-length queries))
  (cond
    [(zero? n) 0]
    [(<= workers 1) (search-sequential-opt index queries)]
    [else
     (define k (max 1 (min workers n)))
     (define chunk-size (quotient n k))
     (define pool (make-parallel-thread-pool k))

     (define threads
       (for/list ([i (in-range k)])
         (define start (* i chunk-size))
         (define end (if (= i (sub1 k)) n (+ start chunk-size)))
         (thread (λ ()
                   (for/sum ([j (in-range start end)])
                     (set-count (process-query-opt index (vector-ref queries j)))))
                 #:pool pool #:keep 'results)))

     (define total (for/sum ([t threads]) (thread-wait t)))
     (parallel-thread-pool-close pool)
     total]))

;; ============================================================
;; Query Generation
;; ============================================================

(define (generate-queries-opt index num-queries query-size)
  (define words (opt-hash-keys-flat index))
  (define word-vec (list->vector words))
  (define num-words (vector-length word-vec))
  (when (zero? num-words)
    (error 'generate-queries "Cannot generate queries from empty index"))
  (for/vector ([_ (in-range num-queries)])
    (define size (+ 2 (random (min query-size (max 1 (- num-words 1))))))
    (for/vector ([_ (in-range size)])
      (vector-ref word-vec (random num-words)))))

;; ============================================================
;; Parallel Build (workers have private state, then merge)
;; ============================================================

(define (build-local-opt-index docs start end)
  ;; Build local index for a chunk of documents
  (define index (make-hasheq))  ; Use standard hash for local (no contention)
  (for ([doc-id (in-range start end)])
    (define doc (vector-ref docs doc-id))
    (define word-counts (make-hasheq))
    (for ([word (in-vector doc)])
      (hash-update! word-counts word add1 0))
    (for ([(word count) (in-hash word-counts)])
      (hash-update! index word
                    (λ (lst) (cons (cons doc-id count) lst))
                    '())))
  index)

(define (merge-into-opt-hash! opt-ht std-ht)
  ;; Merge standard hash into optimistic hash
  (for ([(word postings) (in-hash std-ht)])
    (define existing (opt-hash-ref opt-ht word '()))
    (opt-hash-set! opt-ht word (append postings existing))))

(define (build-opt-index-sequential docs)
  ;; Build directly into optimistic hash
  (define vocab-size (max 1000 (quotient (vector-length docs) 10)))
  (define index (make-opt-hash (* vocab-size 2)))
  (for ([doc (in-vector docs)]
        [doc-id (in-naturals)])
    (define word-counts (make-hasheq))
    (for ([word (in-vector doc)])
      (hash-update! word-counts word add1 0))
    (for ([(word count) (in-hash word-counts)])
      (define existing (opt-hash-ref index word '()))
      (opt-hash-set! index word (cons (cons doc-id count) existing))))
  index)

(define (build-opt-index-parallel docs #:workers [workers (processor-count)])
  ;; Parallel build: local standard hashes, then merge into optimistic hash
  (define n (vector-length docs))
  (cond
    [(zero? n) (make-opt-hash)]
    [(<= workers 1) (build-opt-index-sequential docs)]
    [else
     (define k (max 1 (min workers n)))
     (define chunk-size (quotient n k))
     (define pool (make-parallel-thread-pool k))

     ;; Phase 1: Build local indexes in parallel (no contention)
     (define local-threads
       (for/list ([i (in-range k)])
         (define start (* i chunk-size))
         (define end (if (= i (sub1 k)) n (+ start chunk-size)))
         (thread (λ () (build-local-opt-index docs start end))
                 #:pool pool #:keep 'results)))

     ;; Wait for all local indexes
     (define local-indexes (map thread-wait local-threads))

     ;; Phase 2: Merge into optimistic hash (sequential, but fast)
     (define vocab-size (max 1000 (quotient n 10)))
     (define result (make-opt-hash (* vocab-size 2)))
     (for ([local-idx (in-list local-indexes)])
       (merge-into-opt-hash! result local-idx))

     (parallel-thread-pool-close pool)
     result]))

;; ============================================================
;; Benchmark
;; ============================================================

(module+ main
  (define n 1000000)
  (define workers (processor-count))
  (define repeat 5)
  (define rng-seed 42)

  (void
   (command-line
    #:program "inverted-index-opt.rkt"
    #:once-each
    [("--n") arg "Problem size"
     (set! n (parse-positive-integer arg 'inverted-index-opt))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'inverted-index-opt))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'inverted-index-opt))]
    [("--seed") arg "Random seed"
     (set! rng-seed (parse-positive-integer arg 'inverted-index-opt))]))

  (when rng-seed (random-seed rng-seed))

  (printf "=== Inverted Index with Optimistic Hash ===\n")
  (printf "Problem size: ~a, Workers: ~a\n\n" n workers)

  ;; Generate documents
  (define docs (generate-documents n))
  (define num-docs (vector-length docs))
  (define num-queries (max 100 (quotient n 100)))

  (printf "Documents: ~a, Queries: ~a\n\n" num-docs num-queries)

  ;; ======== BUILD PHASE ========
  (printf "--- BUILD PHASE ---\n")

  ;; Warmup
  (for ([_ 2])
    (build-opt-index-sequential docs)
    (build-opt-index-parallel docs #:workers workers))

  ;; Sequential build
  (collect-garbage) (collect-garbage)
  (define build-seq-times
    (for/list ([_ repeat])
      (define-values (result cpu real gc) (time-apply (λ () (build-opt-index-sequential docs)) '()))
      real))
  (define build-seq-avg (/ (apply + build-seq-times) repeat))
  (printf "Sequential: ~a ms\n" (exact->inexact build-seq-avg))

  ;; Parallel build
  (collect-garbage) (collect-garbage)
  (define build-par-times
    (for/list ([_ repeat])
      (define-values (result cpu real gc) (time-apply (λ () (build-opt-index-parallel docs #:workers workers)) '()))
      real))
  (define build-par-avg (/ (apply + build-par-times) repeat))
  (printf "Parallel:   ~a ms\n" (exact->inexact build-par-avg))
  (printf "Speedup:    ~ax\n\n" (/ (round (* (/ build-seq-avg build-par-avg) 100)) 100.0))

  ;; Build index for query phase
  (define index (build-opt-index-parallel docs #:workers workers))
  (define queries (generate-queries-opt index num-queries 4))

  ;; ======== QUERY PHASE ========
  (printf "--- QUERY PHASE ---\n")

  ;; Warmup
  (for ([_ 2])
    (search-sequential-opt index queries)
    (search-parallel-opt index queries #:workers workers))

  ;; Sequential query
  (collect-garbage) (collect-garbage)
  (define query-seq-times
    (for/list ([_ repeat])
      (define-values (_ cpu real gc) (time-apply (λ () (search-sequential-opt index queries)) '()))
      real))
  (define query-seq-avg (/ (apply + query-seq-times) repeat))
  (printf "Sequential: ~a ms\n" (exact->inexact query-seq-avg))

  ;; Parallel query
  (collect-garbage) (collect-garbage)
  (define query-par-times
    (for/list ([_ repeat])
      (define-values (_ cpu real gc) (time-apply (λ () (search-parallel-opt index queries #:workers workers)) '()))
      real))
  (define query-par-avg (/ (apply + query-par-times) repeat))
  (printf "Parallel:   ~a ms\n" (exact->inexact query-par-avg))
  (printf "Speedup:    ~ax\n\n" (/ (round (* (/ query-seq-avg query-par-avg) 100)) 100.0))

  ;; ======== COMBINED ========
  (printf "--- COMBINED (BUILD + QUERY) ---\n")
  (define total-seq (+ build-seq-avg query-seq-avg))
  (define total-par (+ build-par-avg query-par-avg))
  (printf "Sequential: ~a ms\n" (exact->inexact total-seq))
  (printf "Parallel:   ~a ms\n" (exact->inexact total-par))
  (printf "Speedup:    ~ax\n" (/ (round (* (/ total-seq total-par) 100)) 100.0)))
