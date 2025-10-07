#lang racket

(require racket/match
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
(define (meeting-place meeting-ch n)
  (thread
   (lambda ()
     (let loop ([n n])
       (if (zero? n)
           ;; Fade all creatures
           (let loop ()
             (let ([c (channel-get meeting-ch)])
               (channel-put (car c) #f)
               (loop)))
           ;; Let two creatures meet
           (match-let ([(cons ch1 v1) (channel-get meeting-ch)]
                       [(cons ch2 v2) (channel-get meeting-ch)])
             (channel-put ch1 v2)
             (channel-put ch2 v1)
             (loop (sub1 n))))))))

;; Individual creature
(define (creature color meeting-ch result-ch)
  (thread
   (lambda ()
     (let ([ch (make-channel)]
           [name (gensym)])
       (let loop ([color color] [met 0] [same 0])
         (channel-put meeting-ch (cons ch (cons color name)))
         (match (channel-get ch)
           [(cons other-color other-name)
            ;; Meet another creature
            (sleep 0) ; avoid imbalance from weak fairness
            (loop (change-color color other-color)
                  (add1 met)
                  (+ same (if (eq? name other-name) 1 0)))]
           [#f
            ;; Done - return results
            (channel-put result-ch (cons met same))]))))))

(define (run-chameneos n colors)
  (define result-ch (make-channel))
  (define meeting-ch (make-channel))

  (meeting-place meeting-ch n)

  (for ([color colors])
    (creature color meeting-ch result-ch))

  (define results
    (for/list ([_ colors])
      (channel-get result-ch)))

  (list (length results)
        (apply + (map car results))
        (apply + (map cdr results))))

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
