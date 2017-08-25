#lang sicp

; Exercise 1.16
;
; Design a procedure that evolves an iterative exponentiation process that uses successive squaring
; and uses a logarithmic number of steps, as does fast-expt.

; Solution

(define (expt-rec b n)
  (expt-iter b n 1))

(define (expt-iter b n accum)
  (cond [(= n 0) accum]
        [(even? n) (expt-iter (square b) (/ n 2) accum)]
        [else (expt-iter b (- n 1) (* accum b))]))

(module+ test
  (require rackunit)

  (check-equal? (expt 2 0) 1)
  (check-equal? (expt 2 1) 2)
  (check-equal? (expt 2 2) 4)
  (check-equal? (expt 2 3) 8)
  (check-equal? (expt 2 4) 16)
  (check-equal? (expt 2 5) 32)
  (check-equal? (expt 2 6) 64)
  (check-equal? (expt 2 7) 128)
  (check-equal? (expt 2 8) 256)
  (check-equal? (expt 2 9) 512)
  (check-equal? (expt 2 10) 1024))
