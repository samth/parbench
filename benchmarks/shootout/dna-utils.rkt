#lang racket

(require racket/list)

(provide alu-sequence
         iub-frequencies
         homo-sapiens-frequencies
         make-lcg
         generate-repeat-sequence
         generate-random-sequence
         build-cdf
         build-dna-sample)

(define alu-sequence
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACAAGGTCAGGAGATCGAGACCATCCTGGCTAACAAGGTTCCCCTGCCTCAGCCTCCCGAGTAGCTGGGACTACAGGCGTGCGCCACCACGCCTGGCTAATTTTTGTATTTTTAGTAGAGACGGGGTTTCACCATGTTGGCCAGGCTGGTCTCGAACTCCTGACCTCAGGTGATCCGCCCGCCTCGGCCTCCCAAAGTGCTGGGATTACAGGCGTGAGCCACTGCACCCGGG")

(define iub-frequencies
  '((#\a . 0.27)
    (#\c . 0.12)
    (#\g . 0.12)
    (#\t . 0.27)
    (#\B . 0.02)
    (#\D . 0.02)
    (#\H . 0.02)
    (#\K . 0.02)
    (#\M . 0.02)
    (#\N . 0.02)
    (#\R . 0.02)
    (#\S . 0.02)
    (#\V . 0.02)
    (#\W . 0.02)
    (#\Y . 0.02)))

(define homo-sapiens-frequencies
  '((#\a . 0.3029549426680)
    (#\c . 0.1979883004921)
    (#\g . 0.1975473066391)
    (#\t . 0.3015094502008)))

(define (make-lcg seed)
  (define state seed)
  (λ ()
    (set! state (modulo (+ (* state 3877) 29573) 139968))
    (/ state 139968.0)))

(define (build-cdf freqs)
  (define total 0.0)
  (for/list ([entry (in-list freqs)])
    (set! total (+ total (cdr entry)))
    (cons total (car entry))))

(define (generate-repeat-sequence length base)
  (define base-length (string-length base))
  (define result (make-string length))
  (for ([i (in-range length)])
    (define ch (string-ref base (modulo i base-length)))
    (string-set! result i ch))
  result)

(define (generate-random-sequence length freqs rng)
  (define cdf (build-cdf freqs))
  (define result (make-string length))
  (define last-char (cdr (last cdf)))
  (for ([i (in-range length)])
    (define r (rng))
    (define ch
      (let loop ([entries cdf]
                 [fallback last-char])
        (cond
          [(null? entries) fallback]
          [(< r (caar entries)) (cdar entries)]
          [else (loop (cdr entries) fallback)])))
    (string-set! result i ch))
  result)

(define (build-dna-sample n)
  (define rng (make-lcg 42))
  (define alu (generate-repeat-sequence (* 2 n) alu-sequence))
  (define iub (generate-random-sequence (* 3 n) iub-frequencies rng))
  (define human (generate-random-sequence (* 5 n) homo-sapiens-frequencies rng))
  (string-upcase (string-append alu iub human)))
