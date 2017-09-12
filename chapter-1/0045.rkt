#lang sicp

(require support/fuzzy-checks)
(require (only-in chapter-1/0035 fixed-point))
(require (only-in chapter-1/0043 repeated))

; Exercise 1.45
;
; We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point
; of y -> x/y does not converge, and that this can be fixed by average damping. The same method
; works for finding cube roots as fixed points of the average-damped y -> x/y^2. Unfortunately,
; the process does not work for fourth roots - a single average damp is not enough to make
; a fixed-point search for y -> x/y^3 converge. On the other hand, if we average damp twice
; (i.e., use the average damp of the average damp of y -> x/y^3) the fixed-point search does converge.
; Do some experiments to determine how many average damps are required to compute nth roots as a
; fixed-point search based upon repeated average damping of y -> x/y^(n-1). Use this to implement
; a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated
; procedure of exercise 1.43. Assume that any arithmetic operations you need are available
; as primitives.

; Solution
; Original definitions

(define (average-damp f)
  (lambda (x) (average x (f x))))

; We define a procedure that yields both the n-th root (so that it can be verified) and the number
; of average damps it takes so that f(y) = x/y^(n-1) converges for the fixed value of x.
;
; A simplification of Floyd's Tortoise and Hare cycle detection algorithm is used to verify
; that the repeated application of the k-fold average-damped function f does not have cycles
; (as long the successive values are distant enough from each other).

(define (algebraic-root n x initial)
  (define (no-cycles? f)
    (define double-f (repeated f 2))
    (define (iter x double-x)
      (let ([next (f x)])
        (cond [(= x double-x) false]
              [(fuzzy-equal? x next) true]
              [else (iter next (double-f double-x))])))

    (iter initial (f initial)))

  (define (iter f number-of-damps)
    (if (no-cycles? f)
        (values (fixed-point f initial) number-of-damps)
        (iter (average-damp f) (+ number-of-damps 1))))

  (iter (lambda (y) (/ x (expt y (- n 1)))) 0))

(module+ test
  (require rackunit)

  (let-values ([(root number-of-damps) (algebraic-root 2 4 1.0)])
    (check-= root 2 default-tolerance)
    (check-equal? number-of-damps 1))

  (let-values ([(root number-of-damps) (algebraic-root 3 8 1.0)])
    (check-= root 2 default-tolerance)
    (check-equal? number-of-damps 1))

  (let-values ([(root number-of-damps) (algebraic-root 5 1024 1.0)])
    (check-= root 4 default-tolerance)
    (check-equal? number-of-damps 2))

  (let-values ([(root number-of-damps) (algebraic-root 5 3125 1.0)])
    (check-= root 5 default-tolerance)
    (check-equal? number-of-damps 2)))
