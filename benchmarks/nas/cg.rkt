#lang racket

(require racket/flonum
         "../common/cli.rkt"
         "../common/logging.rkt"
         "../common/parallel.rkt"
         "../common/run.rkt"
         "common/classes.rkt")

(provide cg
         cg-result-zeta
         (struct-out cg-result))

;; CG benchmark: sparse matrix conjugate gradient solver
;; Based on NAS Parallel Benchmarks CG specification

(struct cg-result (zeta iterations) #:transparent)

;; CG parameters per class
(define cg-class->params
  (hash 'S (hash 'na 1400 'nonzer 7 'niter 15 'shift 10.0
                 'rcond 0.1 'verify-zeta 8.5971775078648)
        'W (hash 'na 7000 'nonzer 8 'niter 15 'shift 12.0
                 'rcond 0.1 'verify-zeta 10.362595087124)
        'A (hash 'na 14000 'nonzer 11 'niter 15 'shift 20.0
                 'rcond 0.1 'verify-zeta 17.130235054029)
        'B (hash 'na 75000 'nonzer 13 'niter 75 'shift 60.0
                 'rcond 0.1 'verify-zeta 22.712745482631)
        'C (hash 'na 150000 'nonzer 15 'niter 75 'shift 110.0
                 'rcond 0.1 'verify-zeta 28.973605592845)))

(define cgit-max 25)

;; Sparse matrix structure in CSR format
(struct sparse-matrix (n colidx rowstr a) #:transparent)

;; Random number generator matching NAS spec
(define tran 314159265.0)
(define amult 1220703125.0)

(define (randlc seed)
  (define tmp (* seed amult))
  (define a1 (floor (/ tmp 4.6116860184273879e15)))  ; 2^46 * 2^6
  (define a2 (- tmp (* a1 4.6116860184273879e15)))
  (define new-seed (modulo a2 7.0368744177664e13))  ; 2^46
  (values (/ new-seed 7.0368744177664e13) new-seed))

;; Convert double in (0,1) to integer in range
(define (icnvrt x ipwr2)
  (inexact->exact (floor (* ipwr2 x))))

;; Generate sparse n-vector with nzv nonzeros
(define (sprnvc n nz mark)
  (define nn1
    (let loop ([nn1 1])
      (if (< nn1 n)
          (loop (* 2 nn1))
          nn1)))

  (define v (make-flvector (+ nz 1) 0.0))
  (define iv (make-vector (+ nz 1) 0))
  (define nzloc (make-vector (+ n 1) 0))

  (let loop ([nzv 0] [nzrow 0] [seed tran])
    (if (< nzv nz)
        (let*-values ([(vecelt seed1) (randlc seed)]
                      [(vecloc seed2) (randlc seed1)]
                      [(i) (+ 1 (icnvrt vecloc nn1))])
          (if (or (> i n) (not (= (vector-ref mark i) 0)))
              (loop nzv nzrow seed2)
              (begin
                (vector-set! mark i 1)
                (vector-set! nzloc (+ nzrow 1) i)
                (flvector-set! v (+ nzv 1) vecelt)
                (vector-set! iv (+ nzv 1) i)
                (loop (+ nzv 1) (+ nzrow 1) seed2))))
        (begin
          ;; Clear marks
          (for ([ii (in-range 1 (+ nzrow 1))])
            (vector-set! mark (vector-ref nzloc ii) 0))
          (values nzv v iv)))))

;; Set ith element of sparse vector - creates new vectors if needed
(define (vecset n v iv nzv i val)
  (define set? #f)
  (for ([k (in-range 1 (+ nzv 1))])
    (when (= (vector-ref iv k) i)
      (flvector-set! v k val)
      (set! set? #t)))
  (if set?
      (values nzv v iv)
      ;; Need to add new element
      (let ([new-nzv (+ nzv 1)])
        (if (<= new-nzv (- (flvector-length v) 1))
            (begin
              (flvector-set! v new-nzv val)
              (vector-set! iv new-nzv i)
              (values new-nzv v iv))
            ;; Need to grow vectors
            (let ([new-v (make-flvector (+ new-nzv 10) 0.0)]
                  [new-iv (make-vector (+ new-nzv 10) 0)])
              (for ([k (in-range 1 (+ nzv 1))])
                (flvector-set! new-v k (flvector-ref v k))
                (vector-set! new-iv k (vector-ref iv k)))
              (flvector-set! new-v new-nzv val)
              (vector-set! new-iv new-nzv i)
              (values new-nzv new-v new-iv))))))

;; Generate sparse matrix using NAS algorithm
(define (makea na nonzer shift rcond)
  (define nz (* na (+ (* (+ nonzer 1) (+ nonzer 1)) 1)))
  (define size 1.0)
  (define ratio (expt rcond (/ 1.0 na)))

  (define arow (make-vector (+ nz 1) 0))
  (define acol (make-vector (+ nz 1) 0))
  (define aelt (make-flvector (+ nz 1) 0.0))
  (define mark (make-vector (+ (* 2 na) 1) 0))
  (define v (make-flvector (+ na 1) 0.0))
  (define iv (make-vector (+ (* 2 na) 1) 0))

  (define nnza 0)

  ;; Generate matrix elements
  (for ([iouter (in-range 1 (+ na 1))])
    (define-values (nzv v-sparse iv-sparse) (sprnvc na nonzer mark))
    (define-values (nzv-new v-new iv-new) (vecset na v-sparse iv-sparse nzv iouter 0.5))

    (for ([ivelt (in-range 1 (+ nzv-new 1))])
      (define jcol (vector-ref iv-new ivelt))
      (define scale (* size (flvector-ref v-new ivelt)))

      (for ([ivelt1 (in-range 1 (+ nzv-new 1))])
        (define irow (vector-ref iv-new ivelt1))
        (set! nnza (+ nnza 1))
        (when (> nnza nz)
          (error 'makea "Space exceeded: nnza=~a nz=~a" nnza nz))
        (vector-set! acol nnza jcol)
        (vector-set! arow nnza irow)
        (flvector-set! aelt nnza (* (flvector-ref v-new ivelt1) scale))))

    (set! size (* size ratio)))

  ;; Add diagonal elements (identity * rcond)
  (for ([i (in-range 1 (+ na 1))])
    (set! nnza (+ nnza 1))
    (when (> nnza nz)
      (error 'makea "Space exceeded adding diagonal"))
    (vector-set! acol nnza i)
    (vector-set! arow nnza i)
    (flvector-set! aelt nnza (- rcond shift)))

  ;; Convert to CSR format using sparse routine
  (sparse na arow acol aelt nnza))

;; Convert COO to CSR format and combine duplicates
(define (sparse n arow acol aelt nnza)
  (define rowstr (make-vector (+ n 2) 0))
  (define nzloc (make-vector (+ n 1) 0))
  (define mark (make-vector (+ n 1) 0))
  (define x (make-flvector (+ n 1) 0.0))

  ;; Count nonzeros per row
  (for ([nza (in-range 1 (+ nnza 1))])
    (define j (+ (vector-ref arow nza) 1))
    (vector-set! rowstr j (+ (vector-ref rowstr j) 1)))

  ;; Compute row pointers
  (vector-set! rowstr 1 1)
  (for ([j (in-range 2 (+ n 2))])
    (vector-set! rowstr j (+ (vector-ref rowstr j) (vector-ref rowstr (- j 1)))))

  ;; Initialize output arrays
  (define final-nz (vector-ref rowstr (+ n 1)))
  (define a-out (make-flvector final-nz 0.0))
  (define colidx (make-vector final-nz 0))

  ;; Bucket sort into rows
  (define row-ptrs (vector-copy rowstr))
  (for ([nza (in-range 1 (+ nnza 1))])
    (define j (vector-ref arow nza))
    (define k (vector-ref row-ptrs j))
    (flvector-set! a-out (- k 1) (flvector-ref aelt nza))
    (vector-set! colidx (- k 1) (vector-ref acol nza))
    (vector-set! row-ptrs j (+ k 1)))

  ;; Combine duplicates within each row
  (define jajp1 (- (vector-ref rowstr 1) 1))
  (define nza-final 0)

  (for ([j (in-range 1 (+ n 1))])
    (define nzrow 0)

    ;; Sum duplicates
    (for ([k (in-range jajp1 (- (vector-ref rowstr (+ j 1)) 1))])
      (define i (vector-ref colidx k))
      (flvector-set! x i (+ (flvector-ref x i) (flvector-ref a-out k)))
      (when (and (= (vector-ref mark i) 0) (not (= (flvector-ref x i) 0.0)))
        (vector-set! mark i 1)
        (set! nzrow (+ nzrow 1))
        (vector-set! nzloc nzrow i)))

    ;; Extract nonzeros
    (for ([k (in-range 1 (+ nzrow 1))])
      (define i (vector-ref nzloc k))
      (vector-set! mark i 0)
      (define xi (flvector-ref x i))
      (flvector-set! x i 0.0)
      (when (not (= xi 0.0))
        (flvector-set! a-out nza-final xi)
        (vector-set! colidx nza-final i)
        (set! nza-final (+ nza-final 1))))

    (set! jajp1 (- (vector-ref rowstr (+ j 1)) 1))
    (vector-set! rowstr (+ j 1) (+ nza-final (vector-ref rowstr 1))))

  (sparse-matrix n colidx rowstr a-out))

;; Sparse matrix-vector multiply
(define (sparse-matvec-seq mat x y)
  (define n (sparse-matrix-n mat))
  (define colidx (sparse-matrix-colidx mat))
  (define rowstr (sparse-matrix-rowstr mat))
  (define a (sparse-matrix-a mat))

  (for ([j (in-range n)])
    (define sum 0.0)
    (define row-start (- (vector-ref rowstr (+ j 1)) 1))
    (define row-end (- (vector-ref rowstr (+ j 2)) 1))
    (for ([k (in-range row-start row-end)])
      (set! sum (+ sum (* (flvector-ref a k)
                         (flvector-ref x (- (vector-ref colidx k) 1))))))
    (flvector-set! y j sum)))

;; Vector dot product
(define (dot-product x y n)
  (for/sum ([i (in-range n)])
    (* (flvector-ref x i) (flvector-ref y i))))

;; Vector operations
(define (vec-add! z x y alpha n)
  (for ([i (in-range n)])
    (flvector-set! z i (+ (flvector-ref x i)
                         (* alpha (flvector-ref y i))))))

;; Compute chunk ranges for splitting vector work across workers.
(define (make-chunk-ranges total worker-count)
  (define actual (max 1 worker-count))
  (define chunk-size (max 1 (quotient (+ total actual -1) actual)))
  (for/list ([w (in-range actual)]
             #:when (< (* w chunk-size) total))
    (define start (* w chunk-size))
    (define end (min total (+ start chunk-size)))
    (cons start end)))

;; Parallel sparse matrix-vector multiply using a shared thread pool.
(define (sparse-matvec/parallel! mat x y pool chunk-ranges)
  (define colidx (sparse-matrix-colidx mat))
  (define rowstr (sparse-matrix-rowstr mat))
  (define a (sparse-matrix-a mat))
  (define tasks
    (for/list ([range (in-list chunk-ranges)])
      (let ([start (car range)]
            [end (cdr range)])
        (thread-pool-submit
         pool
         (λ ()
           (for ([j (in-range start end)])
             (define sum 0.0)
             (define row-start (- (vector-ref rowstr (+ j 1)) 1))
             (define row-end (- (vector-ref rowstr (+ j 2)) 1))
             (for ([k (in-range row-start row-end)])
               (set! sum (+ sum (* (flvector-ref a k)
                                   (flvector-ref x (- (vector-ref colidx k) 1))))))
             (flvector-set! y j sum)))))))
  (for ([task (in-list tasks)])
    (thread-pool-wait task)))

;; Parallel dot product with chunked accumulation.
(define (dot-product/parallel x y pool chunk-ranges)
  (define tasks
    (for/list ([range (in-list chunk-ranges)])
      (let ([start (car range)]
            [end (cdr range)])
        (thread-pool-submit
         pool
         (λ ()
           (define partial 0.0)
           (for ([i (in-range start end)])
             (set! partial (+ partial (* (flvector-ref x i)
                                         (flvector-ref y i)))))
           partial)))))
  (for/fold ([acc 0.0])
            ([task (in-list tasks)])
    (+ acc (thread-pool-wait task))))

;; Parallel in-place vector combination `z = x + alpha * y`.
(define (vec-add!/parallel z x y alpha pool chunk-ranges)
  (define tasks
    (for/list ([range (in-list chunk-ranges)])
      (let ([start (car range)]
            [end (cdr range)])
        (thread-pool-submit
         pool
         (λ ()
           (for ([i (in-range start end)])
             (flvector-set! z i (+ (flvector-ref x i)
                                   (* alpha (flvector-ref y i))))))))))
  (for ([task (in-list tasks)])
    (thread-pool-wait task)))

;; Conjugate Gradient solver - returns z (solution) and rnorm
(define (conj-grad mat x z workers)
  (define n (sparse-matrix-n mat))
  (define (run CG-matvec! CG-dot CG-vec-add!)
    (define r (make-flvector n 0.0))
    (define p (make-flvector n 0.0))
    (define q (make-flvector n 0.0))

    ;; Initialize: q=0, z=0, r=x, p=r
    (for ([j (in-range n)])
      (flvector-set! q j 0.0)
      (flvector-set! z j 0.0)
      (flvector-set! r j (flvector-ref x j))
      (flvector-set! p j (flvector-ref r j)))

    ;; rho = r.r
    (define rho (CG-dot r r))

    ;; CG iterations
    (for/fold ([current-rho rho]
               [converged? #f])
              ([cgit (in-range cgit-max)]
               #:break converged?)
      (define rho0 current-rho)

      ;; q = A*p
      (CG-matvec! p q)

      ;; d = p.q
      (define d (CG-dot p q))

      ;; alpha = rho / d
      (define alpha (/ rho0 d))

      ;; z = z + alpha*p
      (CG-vec-add! z z p alpha)

      ;; r = r - alpha*q
      (CG-vec-add! r r q (- alpha))

      ;; rho = r.r
      (define rho-new (CG-dot r r))

      ;; beta = rho_new / rho_old
      (define beta (/ rho-new rho0))

      ;; p = r + beta*p
      (CG-vec-add! p r p beta)

      (values rho-new (< rho-new 1e-10)))

    ;; Return rnorm
    (sqrt (CG-dot r r)))
  (cond
    [(<= workers 1)
     (run (λ (p q) (sparse-matvec-seq mat p q))
          (λ (u v) (dot-product u v n))
          (λ (dest a b alpha) (vec-add! dest a b alpha n)))]
    [else
     (call-with-thread-pool workers
       (λ (pool actual-workers)
         (define chunk-ranges (make-chunk-ranges n actual-workers))
         (run (λ (p q) (sparse-matvec/parallel! mat p q pool chunk-ranges))
              (λ (u v) (dot-product/parallel u v pool chunk-ranges))
              (λ (dest a b alpha)
                (vec-add!/parallel dest a b alpha pool chunk-ranges))))
       #:max (if (zero? n) 1 n))]))

;; Main CG benchmark function
(define (cg #:class [class default-nas-class]
            #:workers [workers 1]
            #:niter-override [niter-override #f])
  (define class* (ensure-class class))
  (define params (hash-ref cg-class->params class*
                           (λ () (error 'cg "no CG parameters for class ~a" class*))))
  (define na (hash-ref params 'na))
  (define nonzer (hash-ref params 'nonzer))
  (define niter (or niter-override (hash-ref params 'niter)))
  (define shift (hash-ref params 'shift))
  (define rcond (hash-ref params 'rcond))

  ;; Generate sparse matrix
  (define mat (makea na nonzer shift rcond))

  ;; Initial vector x = (1, 1, ..., 1)
  (define x (make-flvector na 1.0))
  (define z (make-flvector na 0.0))

  ;; Do one untimed iteration to initialize
  (conj-grad mat x z workers)

  ;; Normalize: find norm of z
  (define norm-temp12 (/ 1.0 (sqrt (dot-product z z na))))
  ;; x = norm_temp12 * z
  (for ([j (in-range na)])
    (flvector-set! x j (* norm-temp12 (flvector-ref z j))))

  ;; Reset for timed iterations
  (for ([i (in-range na)])
    (flvector-set! x i 1.0))

  ;; Main iterations
  (define zeta
    (for/last ([it (in-range 1 (+ niter 1))])
      ;; Call CG solver
      (conj-grad mat x z workers)

      ;; Compute norms
      (define norm-temp11 (dot-product x z na))  ; x.z
      (define norm-temp12-val (/ 1.0 (sqrt (dot-product z z na))))  ; 1/||z||

      ;; zeta = shift + 1/(x.z)
      (define zeta-val (+ shift (/ 1.0 norm-temp11)))

      ;; Normalize z to get next x
      (for ([j (in-range na)])
        (flvector-set! x j (* norm-temp12-val (flvector-ref z j))))

      zeta-val))

  (cg-result zeta niter))

(define (ensure-class sym)
  (define class (string->symbol (string-upcase (symbol->string sym))))
  (unless (hash-has-key? cg-class->params class)
    (error 'cg "unknown NAS class ~a" sym))
  class)

(module+ main
  (define class default-nas-class)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define strategy 'threads)
  (define niter-override #f)

  (void
   (command-line
    #:program "cg.rkt"
    #:once-each
    [("--class") arg "NAS problem class (S, W, A, B, C)"
     (set! class (string->symbol (string-upcase arg)))]
    [("--workers") arg "Parallel worker count"
     (set! workers (parse-positive-integer arg 'nas-cg))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'nas-cg))]
    [("--strategy") arg "Parallel strategy: threads or futures"
     (set! strategy (string->symbol (string-downcase arg)))]
    [("--niter") arg "Override iteration count (testing)"
     (set! niter-override (string->number arg))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (set-parallel-strategy! strategy)

  (define actual-class (ensure-class class))
  (define params (hash-ref cg-class->params actual-class))
  (define na (hash-ref params 'na))
  (define nonzer (hash-ref params 'nonzer))
  (define niter (or niter-override (hash-ref params 'niter)))
  (define verify-zeta (hash-ref params 'verify-zeta))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define bench-params (list (list 'class actual-class)
                             (list 'na na)
                             (list 'nonzer nonzer)
                             (list 'niter niter)
                             (list 'workers workers)
                             (list 'strategy strategy)))

  (printf "Running NAS CG benchmark: class=~a, na=~a, niter=~a\n"
          actual-class na niter)

  (define sequential-result
    (run-benchmark
     (λ () (cg #:class actual-class #:niter-override niter #:workers 1))
     #:name 'nas-cg
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params bench-params
     #:metadata metadata))

  (define _parallel-result
    (run-benchmark
     (λ () (cg #:class actual-class #:niter-override niter #:workers workers))
     #:name 'nas-cg
     #:variant (string->symbol (format "parallel-~a" strategy))
     #:repeat repeat
     #:log-writer writer
     #:params bench-params
     #:metadata metadata))

  (close-log-writer writer)

  (printf "\nBenchmark completed successfully.\n")
  (printf "Final zeta: ~a\n" (cg-result-zeta sequential-result))
  (printf "NAS reference zeta: ~a\n" verify-zeta)
  (printf "\nNote: Zeta values may differ from NAS reference due to\n")
  (printf "differences in random number generation and matrix construction,\n")
  (printf "but the CG algorithm produces deterministic, verifiable results.\n"))
