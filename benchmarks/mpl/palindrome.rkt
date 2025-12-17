#lang racket

;; Port of palindrome from MPL parallel-ml-bench
;; Finds the LONGEST palindromic substring using polynomial hashing
;; Matches MPL's algorithm with parallel binary search at each position

(require racket/fixnum
         racket/unsafe/ops
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide longest-palindrome-sequential
         longest-palindrome-parallel
         generate-test-string)

;; Polynomial hashing parameters (match MPL)
(define P 1045678717)
(define BASE-VAL 500000000)

(define (mod-p v) (modulo v P))
(define (mul-mod a b) (mod-p (* a b)))
(define (add-mod a b) (mod-p (+ a b)))

;; Sequential: Find longest palindrome using polynomial hashing
(define (longest-palindrome-sequential str)
  (define n (string-length str))
  (when (= n 0) (values 0 0))

  ;; Precompute base powers
  (define base-powers (make-vector (fx+ n 1) 1))
  (for ([i (in-range n)])
    (vector-set! base-powers (fx+ i 1) (mul-mod (vector-ref base-powers i) BASE-VAL)))

  ;; Forward hash: H(i,j) = hash of str[i..j)
  (define forward-prefix (make-vector (fx+ n 1) 0))
  (for ([i (in-range n)])
    (define c (char->integer (string-ref str i)))
    (vector-set! forward-prefix (fx+ i 1)
                 (add-mod (vector-ref forward-prefix i)
                          (mul-mod c (vector-ref base-powers i)))))

  (define (forward-hash i j)
    (define last-val (vector-ref forward-prefix j))
    (define first-val (vector-ref forward-prefix i))
    (define offset (vector-ref base-powers (fx- n i 1)))
    (mod-p (* (- last-val first-val) offset)))

  ;; Backward hash: H(i,j) of reversed string
  (define backward-prefix (make-vector (fx+ n 1) 0))
  (for ([i (in-range n)])
    (define c (char->integer (string-ref str (fx- n i 1))))
    (vector-set! backward-prefix (fx+ i 1)
                 (add-mod (vector-ref backward-prefix i)
                          (mul-mod c (vector-ref base-powers i)))))

  (define (backward-hash i j)
    (define ri (fx- n j))
    (define rj (fx- n i))
    (define last-val (vector-ref backward-prefix rj))
    (define first-val (vector-ref backward-prefix ri))
    (define offset (vector-ref base-powers (fx- n ri 1)))
    (mod-p (* (- last-val first-val) offset)))

  ;; Check if str[i..j) is palindrome using hashes
  (define (perhaps? i j)
    (fx= (forward-hash i j) (backward-hash i j)))

  ;; Binary search for longest palindrome length
  (define (binary-search check-fn)
    (define (bs lo hi)
      (if (fx= (fx- hi lo) 1)
          lo
          (let ([mid (fxquotient (fx+ lo hi) 2)])
            (if (check-fn mid)
                (bs mid hi)
                (bs lo mid)))))
    (if (not (check-fn 1))
        0
        (let loop ([i 1])
          (if (check-fn (fx* i 2))
              (loop (fx* i 2))
              (bs i (fx* i 2))))))

  ;; Find max for odd-length palindromes centered at each position
  (define (check-odd i j)
    (and (fx>= (fx- i j) 0)
         (fx< (fx+ i j) n)
         (perhaps? (fx- i j) (fx+ i j 1))))

  (define-values (best-io best-lo)
    (for/fold ([best-i 0] [best-l 0])
              ([i (in-range n)])
      (define l (binary-search (lambda (j) (check-odd i j))))
      (if (fx> l best-l)
          (values i l)
          (values best-i best-l))))

  ;; Find max for even-length palindromes
  (define (check-even i j)
    (and (fx>= (fx- i j -1) 0)
         (fx< (fx+ i j) n)
         (perhaps? (fx+ (fx- i j) 1) (fx+ i j 1))))

  (define-values (best-ie best-le)
    (for/fold ([best-i 0] [best-l 0])
              ([i (in-range n)])
      (define l (binary-search (lambda (j) (check-even i j))))
      (if (fx> l best-l)
          (values i l)
          (values best-i best-l))))

  ;; Return position and length of longest
  (if (fx> best-le best-lo)
      (values (fx+ (fx- best-ie best-le) 1) (fx* 2 best-le))
      (values (fx- best-io best-lo) (fx+ (fx* 2 best-lo) 1))))

;; Parallel version with parallel reduce over positions
(define (longest-palindrome-parallel str workers)
  (define n (string-length str))
  (when (= n 0) (values 0 0))

  ;; Precompute base powers
  (define base-powers (make-vector (fx+ n 1) 1))
  (for ([i (in-range n)])
    (vector-set! base-powers (fx+ i 1) (mul-mod (vector-ref base-powers i) BASE-VAL)))

  ;; Forward hash prefix sums
  (define forward-prefix (make-vector (fx+ n 1) 0))
  (for ([i (in-range n)])
    (define c (char->integer (string-ref str i)))
    (vector-set! forward-prefix (fx+ i 1)
                 (add-mod (vector-ref forward-prefix i)
                          (mul-mod c (vector-ref base-powers i)))))

  (define (forward-hash i j)
    (define last-val (vector-ref forward-prefix j))
    (define first-val (vector-ref forward-prefix i))
    (define offset (vector-ref base-powers (fx- n i 1)))
    (mod-p (* (- last-val first-val) offset)))

  ;; Backward hash prefix sums
  (define backward-prefix (make-vector (fx+ n 1) 0))
  (for ([i (in-range n)])
    (define c (char->integer (string-ref str (fx- n i 1))))
    (vector-set! backward-prefix (fx+ i 1)
                 (add-mod (vector-ref backward-prefix i)
                          (mul-mod c (vector-ref base-powers i)))))

  (define (backward-hash i j)
    (define ri (fx- n j))
    (define rj (fx- n i))
    (define last-val (vector-ref backward-prefix rj))
    (define first-val (vector-ref backward-prefix ri))
    (define offset (vector-ref base-powers (fx- n ri 1)))
    (mod-p (* (- last-val first-val) offset)))

  (define (perhaps? i j)
    (fx= (forward-hash i j) (backward-hash i j)))

  (define (binary-search check-fn)
    (define (bs lo hi)
      (if (fx= (fx- hi lo) 1)
          lo
          (let ([mid (fxquotient (fx+ lo hi) 2)])
            (if (check-fn mid) (bs mid hi) (bs lo mid)))))
    (if (not (check-fn 1))
        0
        (let loop ([i 1])
          (if (check-fn (fx* i 2))
              (loop (fx* i 2))
              (bs i (fx* i 2))))))

  (define pool (make-parallel-thread-pool workers))
  (define GRAIN 1000)

  ;; Parallel find max for odd-length palindromes
  (define (find-odd-max lo hi)
    (define (check-odd i j)
      (and (fx>= (fx- i j) 0)
           (fx< (fx+ i j) n)
           (perhaps? (fx- i j) (fx+ i j 1))))
    (for/fold ([best-i 0] [best-l 0])
              ([i (in-range lo hi)])
      (define l (binary-search (lambda (j) (check-odd i j))))
      (if (fx> l best-l) (values i l) (values best-i best-l))))

  (define (find-even-max lo hi)
    (define (check-even i j)
      (and (fx>= (fx- i j -1) 0)
           (fx< (fx+ i j) n)
           (perhaps? (fx+ (fx- i j) 1) (fx+ i j 1))))
    (for/fold ([best-i 0] [best-l 0])
              ([i (in-range lo hi)])
      (define l (binary-search (lambda (j) (check-even i j))))
      (if (fx> l best-l) (values i l) (values best-i best-l))))

  ;; Parallel search for odd palindromes
  (define chunk-size (max GRAIN (quotient n workers)))
  (define odd-channels
    (for/list ([w (in-range workers)])
      (define lo (fx* w chunk-size))
      (define hi (if (fx= w (fx- workers 1)) n (min (fx+ lo chunk-size) n)))
      (if (fx>= lo n)
          #f
          (let ([ch (make-channel)])
            (thread #:pool pool
             (lambda ()
               (define-values (bi bl) (find-odd-max lo hi))
               (channel-put ch (vector bi bl))))
            ch))))

  (define odd-results (filter values (map (lambda (ch) (and ch (channel-get ch))) odd-channels)))
  (define-values (best-io best-lo)
    (for/fold ([bi 0] [bl 0])
              ([r (in-list odd-results)])
      (if (fx> (vector-ref r 1) bl)
          (values (vector-ref r 0) (vector-ref r 1))
          (values bi bl))))

  ;; Parallel search for even palindromes
  (define even-channels
    (for/list ([w (in-range workers)])
      (define lo (fx* w chunk-size))
      (define hi (if (fx= w (fx- workers 1)) n (min (fx+ lo chunk-size) n)))
      (if (fx>= lo n)
          #f
          (let ([ch (make-channel)])
            (thread #:pool pool
             (lambda ()
               (define-values (bi bl) (find-even-max lo hi))
               (channel-put ch (vector bi bl))))
            ch))))

  (define even-results (filter values (map (lambda (ch) (and ch (channel-get ch))) even-channels)))
  (define-values (best-ie best-le)
    (for/fold ([bi 0] [bl 0])
              ([r (in-list even-results)])
      (if (fx> (vector-ref r 1) bl)
          (values (vector-ref r 0) (vector-ref r 1))
          (values bi bl))))

  (parallel-thread-pool-close pool)

  ;; Return position and length of longest
  (if (fx> best-le best-lo)
      (values (fx+ (fx- best-ie best-le) 1) (fx* 2 best-le))
      (values (fx- best-io best-lo) (fx+ (fx* 2 best-lo) 1))))

;; Generate test string - alternating pattern for predictable palindrome
(define (generate-test-string n seed)
  (random-seed seed)
  ;; Generate ababab... pattern which has known longest palindrome
  (build-string n (lambda (i) (if (even? i) #\a #\b))))

(module+ main
  (define n 1000000)
  (define workers (processor-count))
  (define repeat 3)
  (define log-path #f)
  (define seed 42)
  (define skip-sequential #f)

  (void
   (command-line
    #:program "palindrome.rkt"
    #:once-each
    [("--n") arg "String length"
     (set! n (parse-positive-integer arg 'palindrome))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'palindrome))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'palindrome))]
    [("--seed") arg "Random seed"
     (set! seed (parse-positive-integer arg 'palindrome))]
    [("--log") arg "S-expression log path"
     (set! log-path arg)]
    [("--skip-sequential") "Skip sequential variant"
     (set! skip-sequential #t)]))

  (printf "Generating test string (n=~a)...\n" n)
  (define str (generate-test-string n seed))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n)
                       (list 'workers workers)
                       (list 'seed seed)))

  (define seq-result #f)
  (unless skip-sequential
    (printf "Running sequential longest palindrome...\n")
    (set! seq-result
      (run-benchmark
       (λ () (let-values ([(pos len) (longest-palindrome-sequential str)])
               (cons pos len)))
       #:name 'palindrome
       #:variant 'sequential
       #:repeat repeat
       #:log-writer writer
       #:params params
       #:metadata metadata)))

  (printf "Running parallel longest palindrome (workers=~a)...\n" workers)
  (define par-result
    (run-benchmark
     (λ () (let-values ([(pos len) (longest-palindrome-parallel str workers)])
             (cons pos len)))
     #:name 'palindrome
     #:variant 'parallel
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata
     #:check (λ (iteration result)
               (when (and seq-result (not (equal? seq-result result)))
                 (error 'palindrome "parallel result mismatch at iteration ~a" iteration)))))

  (close-log-writer writer)

  (define pos (car par-result))
  (define len (cdr par-result))
  (unless skip-sequential
    (printf "\nVerification: ")
    (if (equal? seq-result par-result)
        (printf "✓ Sequential and parallel results match\n")
        (printf "✗ Results differ!\n")))

  (printf "Longest palindrome: position=~a, length=~a\n" pos len)

  ;; Expected for ababab...: if n is even, longest is (1, n-1), if odd, longest is (0, n)
  (define expected-len (if (even? n) (- n 1) n))
  (printf "Expected length for alternating pattern: ~a\n" expected-len)
  (printf "Correct: ~a\n" (= len expected-len)))
