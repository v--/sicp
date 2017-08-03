#lang sicp

; Exercise 1.8
;
; Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x,
; then a better approximation is given by the value
;
; (x / y^2 + 2 * y) / 3
;
; Use this formula to implement a cube-root procedure analogous to the square-root procedure.

; Solution
; All procedures are analogous to the ones in the previous exercise, except for `improve`.

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (define new-guess (improve guess x))
  (if (good-enough? guess new-guess x)
      new-guess
      (cube-root-iter new-guess x)))

(define (good-enough? guess new-guess x)
  (< (abs (/ (- new-guess guess) guess)) 1e-3))

(define (improve guess x)
  (/
    (+
      (/ x (square guess))
      (* 2 guess))
    3))

(check-= (cube-root 8) 2 1e-3)
(check-= (cube-root 8e+9) 2e+3 1e-3)
(check-= (cube-root 8e-9) 2e-3 1e-3)
