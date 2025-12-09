#lang racket

(require racket/place
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide run-richards-sequential
         run-richards-parallel
         richards-result?
         richards-result-queue-count
         richards-result-hold-count)

;; ---------------- Constants ----------------

(define BUFSIZE 4)

(define IDLE-ID 1)
(define WORKER-ID 2)
(define HANDLER-A-ID 3)
(define HANDLER-B-ID 4)
(define DEVICE-A-ID 5)
(define DEVICE-B-ID 6)

(define KIND-DEV 1000)
(define KIND-WORK 1001)

(define TASK-TABLE-SIZE 10)

(define EXPECTED-QUEUE-COUNT 23246)
(define EXPECTED-HOLD-COUNT 9297)

;; ---------------- Packet ----------------

(struct packet (link ident kind datum data)
  #:mutable #:transparent)

(define (make-packet link ident kind)
  (packet link ident kind 0 (make-vector BUFSIZE 0)))

(define (packet-append! pkt queue)
  (set-packet-link! pkt #f)
  (cond
    [(not queue) pkt]
    [else
     (let loop ([node queue])
       (if (packet-link node)
           (loop (packet-link node))
           (set-packet-link! node pkt)))
     queue]))

;; ---------------- Task State ----------------

(struct task-state (packet-pending? task-waiting? task-holding?)
  #:mutable)

(define (make-task-state-running)
  (task-state #f #f #f))

(define (make-task-state-waiting)
  (task-state #f #t #f))

(define (make-task-state-waiting-with-packet)
  (task-state #t #t #f))

(define (task-state-packet-pending! st)
  (set-task-state-packet-pending?! st #t)
  (set-task-state-task-waiting?! st #f)
  (set-task-state-task-holding?! st #f))

(define (task-state-running! st)
  (set-task-state-packet-pending?! st #f)
  (set-task-state-task-waiting?! st #f)
  (set-task-state-task-holding?! st #f))

(define (task-state-waiting! st)
  (set-task-state-packet-pending?! st #f)
  (set-task-state-task-waiting?! st #t)
  (set-task-state-task-holding?! st #f))

(define (task-state-waiting-with-packet! st)
  (set-task-state-packet-pending?! st #t)
  (set-task-state-task-waiting?! st #t)
  (set-task-state-task-holding?! st #f))

(define (task-state-waiting-with-packet? st)
  (and (task-state-packet-pending? st)
       (task-state-task-waiting? st)
       (not (task-state-task-holding? st))))

(define (task-state-holding-or-waiting? st)
  (or (task-state-task-holding? st)
      (and (not (task-state-packet-pending? st))
           (task-state-task-waiting? st))))

;; ---------------- Task Work Area ----------------

(struct task-work-area (task-table task-list hold-count queue-count)
  #:mutable)

(define (make-task-work-area)
  (task-work-area (make-vector TASK-TABLE-SIZE #f) #f 0 0))

(define (task-work-area-reset! twa)
  (vector-fill! (task-work-area-task-table twa) #f)
  (set-task-work-area-task-list! twa #f)
  (set-task-work-area-hold-count! twa 0)
  (set-task-work-area-queue-count! twa 0))

;; ---------------- Task Records ----------------

(struct idle-record (control count) #:mutable)
(struct worker-record (destination count) #:mutable)
(struct handler-record (work-in device-in) #:mutable)
(struct device-record (pending) #:mutable)

;; ---------------- Task ----------------

(struct task (link ident priority input tstate record kind area)
  #:mutable #:transparent)

(define (register-task! twa t)
  (set-task-link! t (task-work-area-task-list twa))
  (set-task-work-area-task-list! twa t)
  (vector-set! (task-work-area-task-table twa) (task-ident t) t)
  t)

(define (task-add-packet! receiver pkt sender)
  (if (not (task-input receiver))
      (begin
        (set-task-input! receiver pkt)
        (set-task-state-packet-pending?! (task-tstate receiver) #t)
        (if (> (task-priority receiver) (task-priority sender))
            receiver
            sender))
      (begin
        (set-task-input! receiver (packet-append! pkt (task-input receiver)))
        sender)))

(define (task-hold! t)
  (define twa (task-area t))
  (set-task-work-area-hold-count! twa (add1 (task-work-area-hold-count twa)))
  (set-task-state-task-holding?! (task-tstate t) #t)
  (task-link t))

(define (task-release! t ident)
  (define target (task-find ident (task-area t)))
  (set-task-state-task-holding?! (task-tstate target) #f)
  (if (> (task-priority target) (task-priority t))
      target
      t))

(define (task-wait! t)
  (set-task-state-task-waiting?! (task-tstate t) #t)
  t)

(define (task-qpkt! t pkt)
  (define twa (task-area t))
  (define target (task-find (packet-ident pkt) twa))
  (set-task-work-area-queue-count! twa (add1 (task-work-area-queue-count twa)))
  (set-packet-link! pkt #f)
  (set-packet-ident! pkt (task-ident t))
  (task-add-packet! target pkt t))

(define (task-find ident twa)
  (define t (vector-ref (task-work-area-task-table twa) ident))
  (unless t
    (error 'richards "unknown task id ~a" ident))
  t)

(define (task-run! t)
  (define st (task-tstate t))
  (define msg
    (if (task-state-waiting-with-packet? st)
        (let ([pkt (task-input t)])
          (set-task-input! t (and pkt (packet-link pkt)))
          (if (task-input t)
              (task-state-packet-pending! st)
              (task-state-running! st))
          (when pkt (set-packet-link! pkt #f))
          pkt)
        #f))
  (dispatch-task t msg))

;; ---------------- Task Dispatch ----------------

(define (dispatch-task t pkt)
  (case (task-kind t)
    [(idle) (idle-task-run t pkt)]
    [(worker) (worker-task-run t pkt)]
    [(handler) (handler-task-run t pkt)]
    [(device) (device-task-run t pkt)]
    [else (error 'richards "unknown task kind" (task-kind t))]))

(define (idle-task-run t pkt)
  (void pkt)
  (define rec (task-record t))
  (define remaining (sub1 (idle-record-count rec)))
  (set-idle-record-count! rec remaining)
  (if (zero? remaining)
      (task-hold! t)
      (let* ([control (idle-record-control rec)]
             [half (quotient control 2)])
        (if (zero? (bitwise-and control 1))
            (begin
              (set-idle-record-control! rec half)
              (task-release! t DEVICE-A-ID))
            (begin
              (set-idle-record-control! rec (bitwise-xor half #xD008))
              (task-release! t DEVICE-B-ID))))))

(define (worker-task-run t pkt)
  (define rec (task-record t))
  (if (not pkt)
      (task-wait! t)
      (let* ([dest (if (= (worker-record-destination rec) HANDLER-A-ID)
                       HANDLER-B-ID
                       HANDLER-A-ID)]
             [_ (set-worker-record-destination! rec dest)]
             [count (worker-record-count rec)])
        (set-packet-ident! pkt dest)
        (set-packet-datum! pkt 0)
        (for ([i (in-range BUFSIZE)])
          (set! count (add1 count))
          (when (> count 26) (set! count 1))
          (vector-set! (packet-data pkt) i (+ (char->integer #\A) count -1)))
        (set-worker-record-count! rec count)
        (task-qpkt! t pkt))))

(define (handler-task-run t pkt)
  (define rec (task-record t))
  (when pkt
    (if (= (packet-kind pkt) KIND-WORK)
        (set-handler-record-work-in! rec (packet-append! pkt (handler-record-work-in rec)))
        (set-handler-record-device-in! rec (packet-append! pkt (handler-record-device-in rec)))))
  (define work (handler-record-work-in rec))
  (if (not work)
      (task-wait! t)
      (let ([count (packet-datum work)])
        (if (>= count BUFSIZE)
            (begin
              (set-handler-record-work-in! rec (packet-link work))
              (task-qpkt! t work))
            (let ([dev (handler-record-device-in rec)])
              (if (not dev)
                  (task-wait! t)
                  (begin
                    (set-handler-record-device-in! rec (packet-link dev))
                    (set-packet-datum! dev (vector-ref (packet-data work) count))
                    (set-packet-datum! work (add1 count))
                    (task-qpkt! t dev))))))))

(define (device-task-run t pkt)
  (define rec (task-record t))
  (if pkt
      (begin
        (set-device-record-pending! rec pkt)
        (task-hold! t))
      (let ([pending (device-record-pending rec)])
        (if pending
            (begin
              (set-device-record-pending! rec #f)
              (task-qpkt! t pending))
            (task-wait! t)))))

;; ---------------- Scheduler ----------------

(define (schedule twa)
  (let loop ([current (task-work-area-task-list twa)])
    (cond
      [(not current)
       (values (task-work-area-queue-count twa)
               (task-work-area-hold-count twa))]
      [(task-state-holding-or-waiting? (task-tstate current))
       (loop (task-link current))]
      [else
       (loop (task-run! current))])))

;; ---------------- Task Constructors ----------------

(define (add-idle-task! twa count)
  (register-task! twa
                  (task #f IDLE-ID 0 #f (make-task-state-running)
                        (idle-record 1 count) 'idle twa)))

(define (add-worker-task! twa queue)
  (register-task! twa
                  (task #f WORKER-ID 1000 queue
                        (make-task-state-waiting-with-packet)
                        (worker-record HANDLER-A-ID 0)
                        'worker twa)))

(define (add-handler-task! twa ident priority queue)
  (register-task! twa
                  (task #f ident priority queue
                        (make-task-state-waiting-with-packet)
                        (handler-record #f #f)
                        'handler twa)))

(define (add-device-task! twa ident priority)
  (register-task! twa
                  (task #f ident priority #f
                        (make-task-state-waiting)
                        (device-record #f)
                        'device twa)))

;; ---------------- Public API ----------------

(struct richards-result (queue-count hold-count)
  #:transparent)

(define (richards-run-once)
  (define twa (make-task-work-area))
  (task-work-area-reset! twa)
  (add-idle-task! twa 10000)
  (define worker-queue
    (make-packet (make-packet #f 0 KIND-WORK) 0 KIND-WORK))
  (add-worker-task! twa worker-queue)
  (define handler-a-queue
    (let ([p1 (make-packet #f DEVICE-A-ID KIND-DEV)]
          [p2 (make-packet #f DEVICE-A-ID KIND-DEV)]
          [p3 (make-packet #f DEVICE-A-ID KIND-DEV)])
      (set-packet-link! p1 p2)
      (set-packet-link! p2 p3)
      p1))
  (add-handler-task! twa HANDLER-A-ID 2000 handler-a-queue)
  (define handler-b-queue
    (let ([p1 (make-packet #f DEVICE-B-ID KIND-DEV)]
          [p2 (make-packet #f DEVICE-B-ID KIND-DEV)]
          [p3 (make-packet #f DEVICE-B-ID KIND-DEV)])
      (set-packet-link! p1 p2)
      (set-packet-link! p2 p3)
      p1))
  (add-handler-task! twa HANDLER-B-ID 3000 handler-b-queue)
  (add-device-task! twa DEVICE-A-ID 4000)
  (add-device-task! twa DEVICE-B-ID 5000)
  (define-values (queue-count hold-count) (schedule twa))
  (richards-result queue-count hold-count))

(define (verify-result res iterations)
  (when (and (> iterations 0)
             (or (not (= (richards-result-queue-count res)
                          (* EXPECTED-QUEUE-COUNT iterations)))
                 (not (= (richards-result-hold-count res)
                          (* EXPECTED-HOLD-COUNT iterations)))))
    (error 'run-richards "unexpected result: ~a" res)))

(define (run-richards-sequential #:iterations [iterations 1])
  (define-values (q h)
    (for/fold ([q 0] [h 0]) ([i (in-range iterations)])
      (define res (richards-run-once))
      (values (+ q (richards-result-queue-count res))
              (+ h (richards-result-hold-count res)))))
  (richards-result q h))

(define (run-richards-parallel #:iterations [iterations 1]
                               #:workers [workers (processor-count)])
  (cond
    [(<= iterations 0) (richards-result 0 0)]
    [else
     (define worker-count (max 1 (min workers iterations)))
     (define chunk (max 1 (quotient (+ iterations worker-count -1) worker-count)))
     (define pool (make-parallel-thread-pool worker-count))

     (define threads
       (for/list ([start (in-range 0 iterations chunk)])
         (define remaining (- iterations start))
         (define count (min remaining chunk))
         (thread (lambda () (run-richards-sequential #:iterations count))
                 #:pool pool #:keep 'results)))

     (define-values (q h)
       (for/fold ([q 0] [h 0]) ([t (in-list threads)])
         (define res (thread-wait t))
         (values (+ q (richards-result-queue-count res))
                 (+ h (richards-result-hold-count res)))))

     (parallel-thread-pool-close pool)
     (richards-result q h)]))

;; ---------------- Benchmark Harness ----------------

(module+ main
  (define iterations 20)
  (define workers (processor-count))
  (define repeat 1)
  (define log-path #f)
  (define skip-sequential #f)

  (void
   (command-line
   #:program "richards.rkt"
   #:once-each
   [("--iterations" "-n") arg "Benchmark iterations per run"
    (set! iterations (parse-positive-integer arg 'richards))]
   [("--workers") arg "Parallel thread count"
    (set! workers (parse-positive-integer arg 'richards))]
   [("--repeat") arg "Benchmark repetitions"
    (set! repeat (parse-positive-integer arg 'richards))]
   [("--log") arg "Optional S-expression log path"
    (set! log-path arg)]
   [("--skip-sequential") "Skip sequential baseline"
    (set! skip-sequential #t)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'iterations iterations)
                       (list 'workers workers)))

  (define sequential-result #f)

  (unless skip-sequential
    (set! sequential-result
          (run-benchmark
           (lambda () (run-richards-sequential #:iterations iterations))
           #:name 'richards
           #:variant 'sequential
           #:repeat repeat
           #:log-writer writer
           #:params params
           #:metadata metadata
           #:check (lambda (_ res) (verify-result res iterations)))))

  (run-benchmark
   (lambda () (run-richards-parallel #:iterations iterations #:workers workers))
   #:name 'richards
   #:variant 'parallel
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata
   #:check (lambda (_ res) (verify-result res iterations)))

  (close-log-writer writer))
