#lang sicp

; Exercise 1.18
; Using the results of exercises 1.16 and 1.17, devise a procedure
; that generates an iterative process for multiplying two integers
; in terms of adding, doubling, and halving and uses a logarithmic
; number of steps.

; Solution

(#%require rackunit)

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (fast-*-iter a b accum)
  (cond [(= b 0) accum]
        [(even? b) (fast-*-iter (double a) (halve b) accum)]
        [else (fast-*-iter a (- b 1) (+ accum a))]))

(define (fast-* a b)
  (fast-*-iter a b 0))

(check-equal? (* 1 2) (fast-* 1 2))
(check-equal? (* 11 22) (fast-* 11 22))
(check-equal? (* 111 222) (fast-* 111 222))
