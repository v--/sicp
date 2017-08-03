#lang sicp

; Exercise 1.36
;
; Modify fixed-point so that it prints the sequence of approximations it generates, using the
; newline and display primitives shown in exercise 1.22. Then find a solution to x^x = 1000
; by finding a fixed point of x -> log(1000)/log(x).
; (Use Scheme's primitive log procedure, which computes natural logarithms.)
; Compare the number of steps this takes with and without average damping.
; (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1) = 0.)

; Solution
; We will divert from the precise exercise requirements to allow automated tests.
; Instead of printing approximations, we return the number of steps required.

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) 1e-5))

(define (fixed-point f first-guess)
  (define (try guess steps)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (values next steps)
          (try next (+ steps 1)))))
  (try first-guess 0))

; We compare the number of steps taken using procedures with and without average dumping.
(define (transform x)
  (/ (log 1000) (log x)))

(let-values ([(x steps) (fixed-point transform 2)])
  (check-= x 4.555 1e-3)
  (check-equal? steps 33))

(define (damped x)
  (/ (+ (/ (log 1000) (log x))
        x)
     2))

(let-values ([(x steps) (fixed-point damped 2)])
  (check-= x 4.555 1e-3)
  (check-equal? steps 8))