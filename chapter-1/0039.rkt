#lang sicp

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
; Original definitions

(define (cont-frac n d k)
  (define (iter m result)
    (if (= m 0)
        result
        (iter (- m 1)
              (/ (n m) (+ (d m) result)))))

  (iter k 0))

; Define a tan procedure

(define (square x)
  (* x x))

(define (tan x k)
  (define minus-square-x (- (square x)))
  (cont-frac (lambda (i) (if (= i 1) x minus-square-x))
             (lambda (i) (+ (* 2 (- i 1)) 1))
             k))

; Verify that the lambda for D_i works
(#%require rackunit)
(#%require racket/math) ; for pi

(check-= (tan 0 100)
         0
         1e-3)

(check-= (tan (/ pi 4) 100)
         1
         1e-3)

(check-= (tan (/ pi 3) 100)
         (/ (sqrt 3) (sqrt 1))
         1e-3)
