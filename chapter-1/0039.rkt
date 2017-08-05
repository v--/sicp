#lang sicp

(require (only-in chapter-1/0037 cont-frac))

; Exercise 1.39
;
; A continued fraction representation of the tangent function was published in 1770
; by the German mathematician J.H. Lambert:
;
; f = _______x_______
;     1 - ____x^2____
;         3 - __x^2__
;             5 - ...
;
; where x is in radians. Define a procedure (tan-cf x k) that computes an approximation
; to the tangent function based on Lambert's formula. k specifies the number of terms to
; compute, as in exercise 1.37.

; Solution
; Define a tan procedure

(define (tan x k)
  (define minus-square-x (- (square x)))
  (cont-frac (lambda (i) (if (= i 1) x minus-square-x))
             (lambda (i) (+ (* 2 (- i 1)) 1))
             k))

; Verify that the lambda for D_i works

(module* test #f
  (require rackunit)

  (check-= (tan 0 100)
           0
           default-tolerance)

  (check-= (tan (/ pi 4) 100)
           1
           default-tolerance)

  (check-= (tan (/ pi 3) 100)
           (/ (sqrt 3) (sqrt 1))
           default-tolerance))
