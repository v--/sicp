#lang sicp

; Exercise 1.32
;
; a. Show that sum and accumulate (exercise 1.31) are both special cases of a still more general
; notion called accumulate that combines a collection of terms, using some general accumulation
; function:
;
; (accumulate combiner null-value term a next b)
;
; Accumulate takes as arguments the same term and range specifications as sum and accumulate,
; together with a combiner procedure (of two arguments) that specifies how the current term
; is to be combined with the accumulation of the preceding terms and a null-value that specifies
; what base value to use when the terms run out. Write accumulate and show how sum and accumulate
; can both be defined as simple calls to accumulate.
;
; b. If your accumulate procedure generates a recursive process, write one that generates
; an iterative process. If it generates an iterative process, write one that generates
; a recursive process.

; Solution

; Recursive process
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; Iterative process
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; First, we define sum and product using both procedures.
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

; Now, verify that the procedures work equivalently.
(define (identity x) x)
(define (double-inc x) (+ x 2))
(check-equal? (sum-iter identity 0 inc 10) 55)
(check-equal? (sum-iter identity 0 double-inc 10) 30)
(check-equal? (product-iter identity 1 inc 5) 120)

(check-equal? (sum-iter identity 1 inc 5)
              (sum identity 1 inc 5))

(check-equal? (product-iter identity 1 inc 5)
              (product identity 1 inc 5))

(define (cube x) (* x x x))
(check-equal? (sum-iter cube 0 inc 10) 3025)

(check-equal? (sum-iter cube 1 inc 3)
              (sum cube 1 inc 3))
