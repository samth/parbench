#lang racket

(require racket/match
         "../common/parallel.rkt"
         "../common/cli.rkt"
         "../common/run.rkt"
         "../common/logging.rkt")

(provide chameneos)

;; Color change rules
(define (change-color c1 c2)
  (case c1
    [(red)
     (case c2 [(blue) 'yellow] [(yellow) 'blue] [else c1])]
    [(yellow)
     (case c2 [(blue) 'red] [(red) 'blue] [else c1])]
    [(blue)
     (case c2 [(yellow) 'red] [(red) 'yellow] [else c1])]))

;; Meeting place coordinator
(define (meeting-place pool meeting-ch total-meetings creature-count)
  (thread
   #:pool pool
   (λ ()
     (let loop ([remaining total-meetings]
                [waiting #f]
                [faded 0])
       (define msg (channel-get meeting-ch))
       (define ch (car msg))
       (define payload (cdr msg))
       (cond
         [(zero? remaining)
          (channel-put ch #f)
          (define next-faded (add1 faded))
          (unless (= next-faded creature-count)
            (loop remaining waiting next-faded))]
         [(not waiting)
          (loop remaining msg faded)]
         [else
          (define other waiting)
          (define other-ch (car other))
          (define other-payload (cdr other))
          (channel-put ch other-payload)
          (channel-put other-ch payload)
          (loop (sub1 remaining) #f faded)])))))

;; Individual creature
(define (creature pool color meeting-ch result-ch)
  (thread
   #:pool pool
   (λ ()
     (define ch (make-channel))
     (define name (gensym))
     (let loop ([current-color color] [met 0] [same 0])
       (channel-put meeting-ch (cons ch (cons current-color name)))
       (match (channel-get ch)
         [(cons other-color other-name)
          (loop (change-color current-color other-color)
                (add1 met)
                (+ same (if (eq? name other-name) 1 0)))]
         [#f
          (channel-put result-ch (cons met same))])))))

(define (run-chameneos n colors)
  (define creature-count (length colors))
  (define meeting-ch (make-channel))
  (define result-ch (make-channel))
  (call-with-thread-pool (max 1 creature-count)
    (λ (pool _actual)
      (define coordinator (meeting-place pool meeting-ch n creature-count))
      (define creatures
        (for/list ([color colors])
          (creature pool color meeting-ch result-ch)))
      (define results
        (for/list ([_ colors])
          (channel-get result-ch)))
      (for ([t creatures]) (thread-wait t))
      (thread-wait coordinator)
      (list creature-count
            (apply + (map car results))
            (apply + (map cdr results))))
    #:max (max 1 (add1 creature-count))))

(define (chameneos n #:colors [colors '(blue red yellow)])
  (run-chameneos n colors))

(module+ main
  (define n 1000)
  (define colors '(blue red yellow))
  (define repeat 1)
  (define log-path #f)

  (void
   (command-line
    #:program "chameneos.rkt"
    #:once-each
    [("--n") arg "Number of meetings"
     (set! n (parse-positive-integer arg 'chameneos))]
    [("--repeat") arg "Benchmark repetitions"
     (set! repeat (parse-positive-integer arg 'chameneos))]
    [("--log") arg "Optional S-expression log path"
     (set! log-path arg)]))

  (define writer (make-log-writer log-path))
  (define metadata (system-metadata))
  (define params (list (list 'n n) (list 'colors (length colors))))

  (run-benchmark
   (λ () (chameneos n #:colors colors))
   #:name 'chameneos
   #:variant 'threaded
   #:repeat repeat
   #:log-writer writer
   #:params params
   #:metadata metadata)

  (close-log-writer writer))
