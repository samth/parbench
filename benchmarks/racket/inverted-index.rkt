#lang racket

;; Inverted Index Benchmark
;;
;; Builds an inverted index mapping words to (document-id, count) pairs.
;; This is a real-world program used in search engines and databases.
;;
;; Inspired by PBBS (Problem Based Benchmark Suite) invertedIndex benchmark.
;; This is a new Racket implementation, not a port from MPL.

(require racket/place
         racket/match
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide inverted-index-sequential
         inverted-index-parallel
         generate-documents
         verify-indexes)

;; Document representation: vector of words (strings)
;; We don't use a struct since each document is just its word vector

;; -------- Zipf Distribution --------
;; Generates word indices following Zipf's law (common in natural text)

(define (zipf-sample vocab-size)
  ;; Returns a word index with Zipf distribution
  ;; Higher indices (common words) are more likely
  (define u (random))
  ;; Use inverse transform sampling with approximation
  (define s 1.0) ; Zipf exponent
  (define h (* (- vocab-size 0.5) (expt (+ 1.0 (- u)) (/ -1.0 s))))
  (max 0 (min (sub1 vocab-size) (inexact->exact (floor h)))))

(define (zipf-sample/simple vocab-size)
  ;; Simpler approximation: power law
  (define u (+ 0.0001 (random)))  ; avoid zero
  (define rank (inexact->exact (floor (/ 1.0 u))))
  (modulo rank vocab-size))

;; -------- Document Generation --------

(define (generate-documents n)
  ;; Generate synthetic documents with Zipf-distributed words
  ;; n = total words to process; derive dimensions from it
  (define num-docs (max 1 (integer-sqrt n)))
  (define doc-size (max 1 (integer-sqrt n)))
  (define vocab-size (max 1000 (quotient n 1000)))
  (define vocab (for/vector ([i vocab-size]) (format "word~a" i)))
  (for/vector ([doc-id (in-range num-docs)])
    (for/vector ([_ (in-range doc-size)])
      (vector-ref vocab (zipf-sample/simple vocab-size)))))

;; -------- Sequential Implementation --------

(define (inverted-index-sequential docs)
  ;; Build inverted index: word -> list of (doc-id . count)
  (define index (make-hash))
  (for ([doc (in-vector docs)]
        [doc-id (in-naturals)])
    ;; Count words in this document
    (define word-counts (make-hash))
    (for ([word (in-vector doc)])
      (hash-update! word-counts word add1 0))
    ;; Add to global index
    (for ([(word count) (in-hash word-counts)])
      (hash-update! index word
                    (λ (lst) (cons (cons doc-id count) lst))
                    '())))
  index)

;; -------- Parallel Implementation --------

(define (build-local-index docs start end)
  ;; Build index for documents from start to end
  (define index (make-hash))
  (for ([doc-id (in-range start end)])
    (define doc (vector-ref docs doc-id))
    (define word-counts (make-hash))
    (for ([word (in-vector doc)])
      (hash-update! word-counts word add1 0))
    (for ([(word count) (in-hash word-counts)])
      (hash-update! index word
                    (λ (lst) (cons (cons doc-id count) lst))
                    '())))
  index)

(define (merge-two-indexes idx1 idx2)
  ;; Merge idx2 into idx1 (mutating idx1)
  (for ([(word postings) (in-hash idx2)])
    (hash-update! idx1 word
                  (λ (existing) (append postings existing))
                  '()))
  idx1)

(define (pair-up xs)
  (let loop ([xs xs] [acc '()])
    (cond
      [(null? xs) (reverse acc)]
      [(null? (cdr xs)) (reverse (cons (list (car xs)) acc))]
      [else (loop (cddr xs) (cons (list (car xs) (cadr xs)) acc))])))

(define (inverted-index-parallel docs #:workers [workers (processor-count)])
  ;; Two-phase parallel algorithm:
  ;; Phase 1: Build local indexes in parallel
  ;; Phase 2: Merge indexes via tree merge
  (define n (vector-length docs))
  (cond
    [(zero? n) (make-hash)]
    [(<= workers 1) (inverted-index-sequential docs)]
    [else
     (define k (max 1 (min workers n)))
     (define chunk-size (quotient n k))
     (define pool (make-parallel-thread-pool k))

     ;; Phase 1: Spawn threads to build local indexes
     (define local-threads
       (for/list ([i (in-range k)])
         (define start (* i chunk-size))
         (define end (if (= i (sub1 k)) n (+ start chunk-size)))
         (thread (λ () (build-local-index docs start end))
                 #:pool pool #:keep 'results)))

     ;; Phase 2: Tree merge - operates on threads, not results
     ;; Each merge thread waits for its input threads, then merges
     (define (tree-merge threads)
       (cond
         [(null? threads) (make-hash)]
         [(null? (cdr threads)) (thread-wait (car threads))]
         [else
          (define next-level
            (for/list ([pr (in-list (pair-up threads))])
              (match pr
                [(list t1 t2)
                 (thread (λ ()
                           (define idx1 (thread-wait t1))
                           (define idx2 (thread-wait t2))
                           (merge-two-indexes idx1 idx2))
                         #:pool pool #:keep 'results)]
                [(list t1) t1])))
          (tree-merge next-level)]))

     (define result (tree-merge local-threads))
     (parallel-thread-pool-close pool)
     result]))

;; -------- Verification --------

(define (verify-indexes seq-index par-index)
  ;; Verify parallel result matches sequential
  (and (= (hash-count seq-index) (hash-count par-index))
       (for/and ([(word seq-postings) (in-hash seq-index)])
         (define par-postings (hash-ref par-index word '()))
         (equal? (sort seq-postings < #:key car)
                 (sort par-postings < #:key car)))))

(define (index-checksum index)
  ;; Compute a checksum for quick verification
  (for/sum ([(word postings) (in-hash index)])
    (+ (string-length word)
       (for/sum ([p (in-list postings)])
         (+ (car p) (cdr p))))))

;; -------- Benchmark --------

(module+ main
  (define n 10000000)
  (define workers (processor-count))
  (define repeat 10)
  (define log-path #f)
  (define skip-sequential #f)
  (define rng-seed 42)

  (void
   (command-line
    #:program "inverted-index.rkt"
    #:once-each
    [("--n") arg "Problem size (total words to process)"
     (set! n (parse-positive-integer arg 'inverted-index))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'inverted-index))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'inverted-index))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential baseline"
     (set! skip-sequential #t)]
    [("--seed") arg "Random seed integer, or 'none' to leave RNG state untouched"
     (set! rng-seed (maybe-parse-seed arg 'inverted-index))]))

  (when rng-seed (random-seed rng-seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))

  ;; Generate documents (outside timing)
  (define docs (generate-documents n))
  (define num-docs (vector-length docs))
  (define doc-size (if (> num-docs 0) (vector-length (vector-ref docs 0)) 0))

  (define params (list (list 'n n)
                       (list 'num-docs num-docs)
                       (list 'doc-size doc-size)
                       (list 'workers workers)))

  (define sequential-checksum #f)

  (unless skip-sequential
    (define seq-result
      (run-benchmark
       (lambda () (inverted-index-sequential docs))
       #:name 'inverted-index
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata))
    (set! sequential-checksum (index-checksum seq-result)))

  (void
   (if sequential-checksum
       (run-benchmark
        (lambda () (inverted-index-parallel docs #:workers workers))
        #:name 'inverted-index
        #:variant 'parallel
        #:repeat repeat
        #:log-writer writer
        #:params params
        #:metadata metadata
        #:check (lambda (_ result)
                  (define par-checksum (index-checksum result))
                  (unless (= par-checksum sequential-checksum)
                    (error 'inverted-index
                           "checksum mismatch: seq=~a par=~a"
                           sequential-checksum par-checksum))))
       (run-benchmark
        (lambda () (inverted-index-parallel docs #:workers workers))
        #:name 'inverted-index
        #:variant 'parallel
        #:repeat repeat
        #:log-writer writer
        #:params params
        #:metadata metadata)))

  (close-log-writer writer))
