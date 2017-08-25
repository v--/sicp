#lang sicp

; Exercise 2.5
;
; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic
; operations if we represent the pair a and b as the integer that is the product 2^a 3^b.
; Give the corresponding definitions of the procedures cons, car, and cdr.

; Solution

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (extract-power z divisor)
  (define (iter rem n)
    (if (divides? divisor rem)
        (iter (/ rem divisor) (+ n 1))
        n))

  (iter z 0))

(define (car z)
  (extract-power z 2))

(define (cdr z)
  (extract-power z 3))

(module+ test
  (require rackunit)

  (define (verify-selectors pair)
    (check-equal? (cons (car pair) (cdr pair))
      pair))

  (verify-selectors (cons 0 1))
  (verify-selectors (cons 3 15))
  (verify-selectors (cons 10 13)))
