#lang sicp

; Exercise 1.35
;
; Show that the golden ratio phi (section 1.2.2) is a fixed point of the transformation x -> 1 + 1/x,
; and use this fact to compute phi by means of the fixed-point procedure.

; Solution
; We first prove that phi is a fixed point of
;   x -> 1 + 1/x.                                                                                 (1)
;
; We are interested in the values for which both sides of the transformation (1) are equal.
; Formulating this as an equation yields
;
;   x = 1 + 1/x,
;   x^2 - x - 1 = 0,
;
; which is a quadratic equation with roots (1 +- sqrt(5)) / 2. Thus (1) has two fixed points, one
; of which is phi.

; Original definitions
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Verify that phi can be approximated using the fixed-point procedure.
(#%require rackunit)

(check-= (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)
         (/ (+ 1 (sqrt 5)) 2)
         1e-3)
