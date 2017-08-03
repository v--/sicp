#lang sicp

; Exercise 1.7
;
; The good-enough? test used in computing square roots will not be very effective
; for finding the square roots of very small numbers. Also, in real computers,
; arithmetic operations are almost always performed with limited precision.
; This makes our test inadequate for very large numbers.
; Explain these statements, with examples showing how the test fails for small and large numbers.
; An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration
; to the next and to stop when the change is a very small fraction of the guess.
; Design a square-root procedure that uses this kind of end test.
; Does this work better for small and large numbers?

; Solution
; Original definitions

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; The `sqrt` procedure fails for sufficiently small numbers, simply because they are too small for the tolerance level.
(check > (- (sqrt 9e-6) 3e-3) 1e-3)

; The `sqrt` procedure also fails for sufficiently large numbers, but because of the limitations
; of floating-point arithmetic. More specifically, numbers stop being "dense" enough
; and the representable differences between two numbers become greater than the tolerance.
; As a side effect, either subtracting numbers that are "close enough" results in zero
; and the `good-enough?` check passes (e.g. 9e+100) or `sqrt-iter` enters infinite recursion
; because the guesses converge at some point, but `good-enough?` still doesn't pass (e.g. 9e+60).
(check > (- (sqrt 9e+100) 3e+50) 1e-3)

; `new-sqrt` uses the heuristic described in the exercise description.
; Reimplementing only `good-enough?` suffices, however `sqrt-iter` is also refactored to pass
; a `new-guess` parameter to `good-enough?` to avoid "improving" the guess twice -
; once in `sqrt-iter` and once in `good-enough?`.

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(define (new-sqrt-iter guess x)
  (define new-guess (improve guess x))
  (if (new-good-enough? guess new-guess x)
      new-guess
      (new-sqrt-iter new-guess x)))

(define (new-good-enough? guess new-guess x)
  (< (abs (/ (- new-guess guess) guess)) 1e-3))

; `new-sqrt` doesn't have the tolerance problem.
(check-= (new-sqrt 9e-6) 3e-3 1e-3)

; Large numbers still have precision problems, but at least the infinite recursion is avoided.
(check-= (new-sqrt 9e+60) 3e+30 1e+20)
