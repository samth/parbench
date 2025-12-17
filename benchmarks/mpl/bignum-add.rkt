#lang racket

;; Port of bignum-add-opt from MPL parallel-ml-bench
;; Uses block-based parallel carry propagation with parallel scan

(require racket/fixnum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide bignum-add-sequential
         bignum-add-parallel
         make-random-bignum)

;; Represent bignum as vector of bytes (base 128, using 7 bits)
;; Bit 7 is reserved for carry indication
(define BASE 128)
(define MASK #x7F)

;; Sequential bignum addition with carry propagation
(define (bignum-add-sequential a b)
  (define n (bytes-length a))
  (define result (make-bytes (fx+ n 1) 0))
  (define carry 0)
  (for ([i (in-range n)])
    (define sum (fx+ (fx+ (bytes-ref a i) (bytes-ref b i)) carry))
    (bytes-set! result i (fxand sum MASK))
    (set! carry (fxrshift sum 7)))
  (bytes-set! result n carry)
  (if (fx> carry 0)
      result
      (subbytes result 0 n)))

;; Block-based parallel addition with carry propagation
;; Matches MPL's bignum-add-opt algorithm:
;; 1. Each block computes local sums with assumed carry-in of 0
;; 2. Track whether block is "copyable" (carry would propagate through unchanged)
;; 3. Use parallel scan to compute actual carries for each block
;; 4. Apply final carries in parallel

(define BLOCK-SIZE 10000)

;; Init function: compute sum assuming no carry-in
(define-syntax-rule (init a-byte b-byte)
  (fx+ a-byte b-byte))

;; Copy function: check if carry propagates unchanged
;; Returns a-byte if carry wouldn't change, else b-byte
(define-syntax-rule (copy a-byte b-byte)
  (if (fx= b-byte MASK) a-byte b-byte))

(define (bignum-add-parallel a b workers)
  (define n (bytes-length a))
  (define num-blocks (fx+ 1 (fxquotient (fx- n 1) BLOCK-SIZE)))

  (define pool (make-parallel-thread-pool workers))

  ;; Phase 1: Each block computes local carries (how carry would propagate through block)
  (define block-carries (make-vector num-blocks 0))

  (define channels-phase1
    (for/list ([block-idx (in-range num-blocks)])
      (define lo (fx* block-idx BLOCK-SIZE))
      (define hi (fxmin (fx+ lo BLOCK-SIZE) n))
      (define ch (make-channel))
      (thread #:pool pool
       (lambda ()
         ;; Compute how a carry would propagate through this block
         (define local-carry
           (let loop ([i lo] [acc 0])
             (if (fx>= i hi)
                 acc
                 (loop (fx+ i 1)
                       (copy acc (init (bytes-ref a i) (bytes-ref b i)))))))
         (channel-put ch (cons block-idx local-carry))))
      ch))

  (for ([ch (in-list channels-phase1)])
    (define result (channel-get ch))
    (vector-set! block-carries (car result) (cdr result)))

  ;; Phase 2: Parallel scan to propagate carries across blocks
  ;; Sequential scan for simplicity (could be parallelized for many blocks)
  (define block-partials (make-vector (fx+ num-blocks 1) 0))
  (for ([i (in-range num-blocks)])
    (vector-set! block-partials (fx+ i 1)
                 (copy (vector-ref block-partials i)
                       (vector-ref block-carries i))))

  (define last-carry (vector-ref block-partials num-blocks))

  ;; Phase 3: Apply carries in parallel
  (define result (make-bytes (fx+ n 1) 0))

  (define channels-phase3
    (for/list ([block-idx (in-range num-blocks)])
      (define lo (fx* block-idx BLOCK-SIZE))
      (define hi (fxmin (fx+ lo BLOCK-SIZE) n))
      (define ch (make-channel))
      (thread #:pool pool
       (lambda ()
         (define incoming-carry (vector-ref block-partials block-idx))
         (let loop ([i lo] [acc incoming-carry])
           (when (fx< i hi)
             (define sum (init (bytes-ref a i) (bytes-ref b i)))
             (define acc-prime (copy acc sum))
             (define this-byte (fxand (fx+ (fxrshift acc 7) sum) MASK))
             (bytes-set! result i this-byte)
             (loop (fx+ i 1) acc-prime)))
         (channel-put ch 'done)))
      ch))

  (for ([ch (in-list channels-phase3)])
    (channel-get ch))

  (parallel-thread-pool-close pool)

  ;; Handle final carry
  (if (fx> last-carry MASK)
      (begin
        (bytes-set! result n 1)
        result)
      (subbytes result 0 n)))

;; Generate random bignum of n bytes
(define (make-random-bignum n seed)
  (random-seed seed)
  (define result (make-bytes n))
  (for ([i (in-range n)])
    (bytes-set! result i (random BASE)))
  result)

;; Compare two bignums for equality
(define (bignum-equal? a b)
  (bytes=? a b))

;; Format bignum for display
(define (format-bignum v [show 5])
  (define n (bytes-length v))
  (if (<= n (* 2 show))
      (format "[~a]" (string-join (for/list ([i (in-range n)])
                                    (~a (bytes-ref v i))) " "))
      (format "[~a ... ~a]"
              (string-join (for/list ([i (in-range show)])
                            (~a (bytes-ref v i))) " ")
              (string-join (for/list ([i (in-range (- n show) n)])
                            (~a (bytes-ref v i))) " "))))

(module+ main
  (define n 10000000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "bignum-add.rkt"
    #:once-each
    [("--n") arg "Number of digits"
     (set! n (parse-positive-integer arg 'bignum-add))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'bignum-add))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'bignum-add))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'bignum-add))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (printf "Generating random bignums (n=~a bytes)...\n" n)
  (define a (make-random-bignum n seed))
  (define b (make-random-bignum n (+ seed 1)))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential bignum addition...\n")
    (set! seq-result
      (run-benchmark
       (λ () (bignum-add-sequential a b))
       #:name 'bignum-add
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel bignum addition (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (bignum-add-parallel a b workers))
     #:name 'bignum-add
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (bignum-equal? seq-result result)))
                 (error 'bignum-add "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (unless skip-sequential
    (printf "\nVerification: ")
    (if (bignum-equal? seq-result par-result)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "\nResult length: ~a bytes\n" (bytes-length par-result))
  (printf "First/last bytes: ~a\n" (format-bignum par-result)))
