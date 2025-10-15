#lang racket

(struct body (x y z vx vy vz mass) #:mutable)

(define pi 3.141592653589793)
(define days-per-year 365.24)
(define solar-mass (* 4 pi pi))

(define bodies
  (vector
   (body 0 0 0 0 0 0 solar-mass)
   (body
    4.84143144246472090e+00
    -1.16032004402742839e+00
    -1.03622044471123109e-01
    (* 1.66007664274403694e-03 days-per-year)
    (* 7.69901118419740425e-03 days-per-year)
    (* -6.90460016972063023e-05 days-per-year)
    (* 9.54791938424326609e-04 solar-mass))
   (body
    8.34336671824457987e+00
    4.12479856412430479e+00
    -4.03523417114321381e-01
    (* -2.76742510726862411e-03 days-per-year)
    (* 4.99852801234917238e-03 days-per-year)
    (* 2.30417297573763929e-05 days-per-year)
    (* 2.85885980666130812e-04 solar-mass))
   (body
    1.28943695621391310e+01
    -1.51111514016986312e+01
    -2.23307578892655734e-01
    (* 2.96460137564761618e-03 days-per-year)
    (* 2.37847173959480950e-03 days-per-year)
    (* -2.96589568540237556e-05 days-per-year)
    (* 4.36624404335156298e-05 solar-mass))
   (body
    1.53796971148509165e+01
    -2.59193146099879641e+01
    1.79258772950371181e-01
    (* 2.68067772490389322e-03 days-per-year)
    (* 1.62824170038242295e-03 days-per-year)
    (* -9.51592254519715870e-05 days-per-year)
    (* 5.15138902046611451e-05 solar-mass))))

(define (offset-momentum! bodies)
  (define momentum (vector 0.0 0.0 0.0))
  (for ([b bodies])
    (vector-set! momentum 0
                 (+ (vector-ref momentum 0) (* (body-mass b) (body-vx b))))
    (vector-set! momentum 1
                 (+ (vector-ref momentum 1) (* (body-mass b) (body-vy b))))
    (vector-set! momentum 2
                 (+ (vector-ref momentum 2) (* (body-mass b) (body-vz b)))))
  (define sun (vector-ref bodies 0))
  (set-body-vx! sun (/ (- (vector-ref momentum 0)) solar-mass))
  (set-body-vy! sun (/ (- (vector-ref momentum 1)) solar-mass))
  (set-body-vz! sun (/ (- (vector-ref momentum 2)) solar-mass)))

(define (energy bodies)
  (define e 0.0)
  (for ([i (in-range (vector-length bodies))])
    (define bi (vector-ref bodies i))
    (set! e (+ e (* 0.5 (body-mass bi)
                      (+ (* (body-vx bi) (body-vx bi))
                         (* (body-vy bi) (body-vy bi))
                         (* (body-vz bi) (body-vz bi))))))
    (for ([j (in-range (add1 i) (vector-length bodies))])
      (define bj (vector-ref bodies j))
      (define dx (- (body-x bi) (body-x bj)))
      (define dy (- (body-y bi) (body-y bj)))
      (define dz (- (body-z bi) (body-z bj)))
      (define distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
      (set! e (- e (/ (* (body-mass bi) (body-mass bj))
                      distance))))))
  e)

(define (advance! bodies dt)
  (for ([i (in-range (vector-length bodies))])
    (define bi (vector-ref bodies i))
    (for ([j (in-range (add1 i) (vector-length bodies))])
      (define bj (vector-ref bodies j))
      (define dx (- (body-x bi) (body-x bj)))
      (define dy (- (body-y bi) (body-y bj)))
      (define dz (- (body-z bi) (body-z bj)))
      (define distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
      (define mag (/ dt (* distance distance distance)))
      (define mass-j (- (* (body-mass bj) mag)))
      (define mass-i (* (body-mass bi) mag))
      (set-body-vx! bi (+ (body-vx bi) (* dx mass-j)))
      (set-body-vy! bi (+ (body-vy bi) (* dy mass-j)))
      (set-body-vz! bi (+ (body-vz bi) (* dz mass-j)))
      (set-body-vx! bj (+ (body-vx bj) (* dx mass-i)))
      (set-body-vy! bj (+ (body-vy bj) (* dy mass-i)))
      (set-body-vz! bj (+ (body-vz bj) (* dz mass-i))))
  (for ([b bodies])
    (set-body-x! b (+ (body-x b) (* dt (body-vx b))))
    (set-body-y! b (+ (body-y b) (* dt (body-vy b))))
    (set-body-z! b (+ (body-z b) (* dt (body-vz b))))))

(module+ main
  (define args (current-command-line-arguments))
  (define n (if (> (vector-length args) 0)
                (string->number (vector-ref args 0))
                1000000))
  (offset-momentum! bodies)
  (printf "~a\n" (energy bodies))
  (for ([i (in-range n)])
    (advance! bodies 0.01))
  (printf "~a\n" (energy bodies)))
