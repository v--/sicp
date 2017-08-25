#lang sicp

(require (only-in chapter-1/0035 fixed-point))

; Exercise 1.40
;
; Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form
;
; (newtons-method (cubic a b c) 1)
;
; to approximate zeros of the cubic x^3 + ax^2 + bx + c.

; Solution
; Original definitions

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx default-tolerance)

; Actual solution
(define (cubic a b c)
  (lambda (x)
    (+ c
       (* x
          (+ b
             (* x
                (+ a x)))))))

(module+ test
  (require rackunit)

  (check-= (newtons-method (cubic 1 1 1) 1)
           -1
           default-tolerance)

  (check-= (newtons-method (cubic 2 2 4) 1)
           -2
           default-tolerance))
