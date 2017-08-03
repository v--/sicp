#lang sicp

; Exercise 1.29
;
; Simpson's Rule is a more accurate method of numerical integration than the method illustrated above.
; Using Simpson's Rule, the integral of a function f between a and b is approximated as
;
;   (h / 3) * (y_0 + 4 * y_1 + 2 * y_2 + 4 * y_3 + 2 * y_4 + ... + 2 * y_(n - 2) + 4 * y_(n - 1) + y_n),
;
; where h = (b - a)/n, for some even integer n, and y_k = f(a + k * h).
; (Increasing n increases the accuracy of the approximation.)
; Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral,
; computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1
; (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.

; Solution
; Original definitions

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; naive-integral is the integral procedure from chapter 3.1, modified to take n instead of dx.
(define (naive-integral f a b n)
  (define dx (/ (- b a) n))
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; simpson-integral is an implementation of the formula from the exercise description.
(define (simpson-integral f a b n)
  (define dx (exact->inexact (/ (- b a) n))) ; force floating-point computation
  (define (iter k)
    (* (f (+ a (* k dx)))
       (cond [(= k 0) 1]
             [(= k n) 1]
             [(even? k) 2]
             [else 4])))

  (* (/ dx 3)
     (sum iter 0 inc n)))

; Verify that both implementations work
(check-= (naive-integral cube 0 1 100) 1/4 1e-3)
(check-= (simpson-integral cube 0 1 100) 1/4 1e-3)

; Verify that the simpson procedure is more accurate
(define-simple-check (check-closer? a b target)
  (< (abs (- a target)) (abs (- b target))))

(check-closer? (simpson-integral cube 0 1 100)
               (naive-integral cube 0 1 100)
               1/4)

(check-closer? (simpson-integral cube 0 1 1000)
               (naive-integral cube 0 1 1000)
               1/4)
