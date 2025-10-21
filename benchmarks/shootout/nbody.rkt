#lang racket

(require racket/flonum
         racket/list
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide nbody-simulation)

;; Constants
(define +pi+ 3.141592653589793)
(define +days-per-year+ 365.24)
(define +solar-mass+ (* 4.0 +pi+ +pi+))
(define +dt+ 0.01)

(struct body (x y z vx vy vz mass) #:mutable)

(define (make-sun) (body 0.0 0.0 0.0 0.0 0.0 0.0 +solar-mass+))
(define (make-jupiter)
  (body 4.84143144246472090
        -1.16032004402742839
        -1.03622044471123109e-1
        (* 1.66007664274403694e-3 +days-per-year+)
        (* 7.69901118419740425e-3 +days-per-year+)
        (* -6.90460016972063023e-5 +days-per-year+)
        (* 9.54791938424326609e-4 +solar-mass+)))

(define (make-saturn)
  (body 8.34336671824457987
        4.12479856412430479
        -4.03523417114321381e-1
        (* -2.76742510726862411e-3 +days-per-year+)
        (* 4.99852801234917238e-3 +days-per-year+)
        (* 2.30417297573763929e-5 +days-per-year+)
        (* 2.85885980666130812e-4 +solar-mass+)))

(define (make-uranus)
  (body 1.28943695621391310e1
        -1.51111514016986312e1
        -2.23307578892655734e-1
        (* 2.96460137564761618e-03 +days-per-year+)
        (* 2.37847173959480950e-03 +days-per-year+)
        (* -2.96589568540237556e-05 +days-per-year+)
        (*  4.36624404335156298e-05 +solar-mass+)))

(define (make-neptune)
  (body 1.53796971148509165e+01
        -2.59193146099879641e+01
        1.79258772950371181e-01
        (* 2.68067772490389322e-03 +days-per-year+)
        (* 1.62824170038242295e-03 +days-per-year+)
        (* -9.51592254519715870e-05 +days-per-year+)
        (* 5.15138902046611451e-05 +solar-mass+)))

(define (copy-body b)
  (body (body-x b) (body-y b) (body-z b)
        (body-vx b) (body-vy b) (body-vz b)
        (body-mass b)))

(define (offset-momentum! system)
  (define px 0.0)
  (define py 0.0)
  (define pz 0.0)
  (for ([b system])
    (set! px (fl+ px (fl* (body-vx b) (body-mass b))))
    (set! py (fl+ py (fl* (body-vy b) (body-mass b))))
    (set! pz (fl+ pz (fl* (body-vz b) (body-mass b)))))
  (define sun (car system))
  (set-body-vx! sun (fl/ (fl- 0.0 px) +solar-mass+))
  (set-body-vy! sun (fl/ (fl- 0.0 py) +solar-mass+))
  (set-body-vz! sun (fl/ (fl- 0.0 pz) +solar-mass+)))

(define (energy system)
  (define e 0.0)
  (for ([o system])
    (set! e (fl+ e (fl* 0.5 (fl* (body-mass o)
                                  (fl+ (fl+ (fl* (body-vx o) (body-vx o))
                                            (fl* (body-vy o) (body-vy o)))
                                       (fl* (body-vz o) (body-vz o)))))))
    (for ([i (member o system)])
      (when (not (eq? o i))
        (define dx (fl- (body-x o) (body-x i)))
        (define dy (fl- (body-y o) (body-y i)))
        (define dz (fl- (body-z o) (body-z i)))
        (define dist (flsqrt (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz))))
        (set! e (fl- e (fl/ (fl* (body-mass o) (body-mass i)) dist))))))
  e)

(define (advance-sequential! system)
  (for ([o system])
    (define o1x (body-x o))
    (define o1y (body-y o))
    (define o1z (body-z o))
    (define om (body-mass o))
    (define vx (body-vx o))
    (define vy (body-vy o))
    (define vz (body-vz o))
    (for ([i (member o system)])
      (when (not (eq? o i))
        (define dx (fl- o1x (body-x i)))
        (define dy (fl- o1y (body-y i)))
        (define dz (fl- o1z (body-z i)))
        (define dist2 (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz)))
        (define mag (fl/ +dt+ (fl* dist2 (flsqrt dist2))))
        (define dxmag (fl* dx mag))
        (define dymag (fl* dy mag))
        (define dzmag (fl* dz mag))
        (define im (body-mass i))
        (set-body-vx! i (fl+ (body-vx i) (fl* dxmag om)))
        (set-body-vy! i (fl+ (body-vy i) (fl* dymag om)))
        (set-body-vz! i (fl+ (body-vz i) (fl* dzmag om)))
        (set! vx (fl- vx (fl* dxmag im)))
        (set! vy (fl- vy (fl* dymag im)))
        (set! vz (fl- vz (fl* dzmag im)))))
    (set-body-vx! o vx)
    (set-body-vy! o vy)
    (set-body-vz! o vz)
    (set-body-x! o (fl+ o1x (fl* +dt+ vx)))
    (set-body-y! o (fl+ o1y (fl* +dt+ vy)))
        (set-body-z! o (fl+ o1z (fl* +dt+ vz)))))

(define (advance-parallel! system workers pool)
  (define system-vec (list->vector system))
  (define body-count (vector-length system-vec))
  (define pair-indexes
    (for*/vector ([i (in-range body-count)]
                  [j (in-range (add1 i) body-count)])
      (cons i j)))
  (define total-pairs (vector-length pair-indexes))
  (when (> total-pairs 0)
    (define chunk-size (max 1 (ceiling (/ total-pairs workers))))
    (define threads
      (for/list ([start (in-range 0 total-pairs chunk-size)])
        (define end (min total-pairs (+ start chunk-size)))
        (thread #:pool pool #:keep 'results
                (λ ()
                  (for/list ([idx (in-range start end)])
                    (match-define (cons oi ii) (vector-ref pair-indexes idx))
                    (define o (vector-ref system-vec oi))
                    (define i (vector-ref system-vec ii))
                    (define dx (fl- (body-x o) (body-x i)))
                    (define dy (fl- (body-y o) (body-y i)))
                    (define dz (fl- (body-z o) (body-z i)))
                    (define dist2 (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz)))
                    (define mag (fl/ +dt+ (fl* dist2 (flsqrt dist2))))
                    (list oi ii (fl* dx mag) (fl* dy mag) (fl* dz mag)))))))
    (define deltas
      (append*
       (for/list ([t (in-list threads)])
         (call-with-values
          (λ () (thread-wait t))
          (λ values
            (cond
              [(null? values) '()]
              [(null? (cdr values)) (car values)]
              [else values]))))))
    (for ([delta deltas])
      (match-define (list oi ii dxmag dymag dzmag) delta)
      (define o (vector-ref system-vec oi))
      (define i (vector-ref system-vec ii))
      (define om (body-mass o))
      (define im (body-mass i))
      (set-body-vx! i (fl+ (body-vx i) (fl* dxmag om)))
      (set-body-vy! i (fl+ (body-vy i) (fl* dymag om)))
      (set-body-vz! i (fl+ (body-vz i) (fl* dzmag om)))
      (set-body-vx! o (fl- (body-vx o) (fl* dxmag im)))
      (set-body-vy! o (fl- (body-vy o) (fl* dymag im)))
      (set-body-vz! o (fl- (body-vz o) (fl* dzmag im)))))
  (for ([idx (in-range (vector-length system-vec))])
    (define b (vector-ref system-vec idx))
    (set-body-x! b (fl+ (body-x b) (fl* +dt+ (body-vx b))))
    (set-body-y! b (fl+ (body-y b) (fl* +dt+ (body-vy b))))
    (set-body-z! b (fl+ (body-z b) (fl* +dt+ (body-vz b))))))

(define (nbody-simulation n #:workers [workers 1])
  (define system (list (make-sun) (make-jupiter) (make-saturn) (make-uranus) (make-neptune)))
  (define sequential? (<= workers 1))
  (define pool (and (not sequential?) (make-parallel-thread-pool workers)))
  (define advance! (if sequential?
                       advance-sequential!
                       (λ (s) (advance-parallel! s workers pool))))
  (dynamic-wind
    (λ () (void))
    (λ ()
      (offset-momentum! system)
      (define e1 (energy system))
      (for ([_ (in-range n)])
        (advance! system))
      (define e2 (energy system))
      (list e1 e2))
    (λ ()
      (when pool (parallel-thread-pool-close pool)))))

(module+ main
  (define n 1000)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "nbody.rkt"
    #:once-each
    [("--n") arg "Number of iterations"
     (set! n (parse-positive-integer arg 'nbody))]
    [("--workers") arg "Parallel thread count"
     (set! workers (parse-positive-integer arg 'nbody))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'nbody))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n) (list 'workers workers)))

  (define sequential
    (run-benchmark
     (λ () (nbody-simulation n #:workers 1))
     #:name 'nbody
     #:variant 'sequential
     #:repeat repeat
     #:log-writer writer
     #:params params
     #:metadata metadata))

  (run-benchmark
   (λ () (nbody-simulation n #:workers workers))
   #:name 'nbody
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (λ (_ value)
             (define tolerance 1e-6)
             (unless (and (= (length value) (length sequential))
                         (for/and ([v value] [s sequential])
                           (< (abs (- v s)) tolerance)))
               (error 'nbody "parallel mismatch: ~a vs ~a" value sequential))))

  (close-log-writer writer))
