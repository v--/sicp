#lang sicp

(require support/fuzzy-checks)

; Exercise 1.46
;
; Several of the numerical methods described in this chapter are instances of an extremely general
; computational strategy known as iterative improvement. Iterative improvement says that,
; to compute something, we start with an initial guess for the answer, test if the guess is
; good enough, and otherwise improve the guess and continue the process using the improved guess
; as the new guess. Write a procedure iterative-improve that takes two procedures as arguments:
; a method for telling whether a guess is good enough and a method for improving a guess.
; iterative-improve should return as its value a procedure that takes a guess as argument and keeps
; improving the guess until it is good enough. Rewrite the sqrt procedure of section 1.1.7
; and the fixed-point procedure of section 1.3.3 in terms of iterative-improve.

; Solution

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))

  (lambda (initial-guess) (iter initial-guess)))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (fuzzy-equal? (square guess) x))

  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f initial-guess)
  (define (improve guess)
    (f guess))

  (define (good-enough? guess)
    (fuzzy-equal? (f guess) guess))

  ((iterative-improve good-enough? improve) initial-guess))

(module+ test
  (require rackunit)

  (check-= (sqrt 4)
           2
           default-tolerance)

  (check-= (sqrt 9)
           3
           default-tolerance)

  (check-= (sqrt 1024)
           32
           default-tolerance)

  (check-= (fixed-point (lambda (x) x) 1.0)
           1
           default-tolerance)

  (define (sqrt-transform x)
    (lambda (y) (/ (+ x (square y)) (* 2 y))))

  (check-= (fixed-point (sqrt-transform 4) 1.0)
           2
           default-tolerance))
