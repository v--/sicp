#lang sicp

; Exercise 1.31
;
; a. The sum procedure is only the simplest of a vast number of similar abstractions that can be
; captured as higher-order procedures. Write an analogous procedure called product that returns
; the product of the values of a function at points over a given range. Show how to define factorial
; in terms of product. Also use product to compute approximations to pi using the formula
;
;   pi / 4 = (2 * 4 * 4 * 6 * 6 * 8 ...) / (3 * 3 * 5 * 5 * 7 * 7 ...)
;
; b. If your product procedure generates a recursive process, write one that generates
; an iterative process. If it generates an iterative process, write one that generates
; a recursive process.

; Solution

; Recursive process
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Iterative process
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; First, verify that the procedures work equivalently.
(define (identity x) x)
(define (double-inc x) (+ x 2))
(check-equal? (product-iter identity 1 inc 5) 120)
(check-equal? (product-iter identity 1 double-inc 5) 15)

(check-equal? (product-iter identity 1 inc 5)
              (product identity 1 inc 5))

(check-equal? (product-iter identity 1 double-inc 5)
              (product identity 1 double-inc 5))

(define (cube x) (* x x x))
(check-equal? (product cube 1 inc 3) 216)

(check-equal? (product-iter cube 1 inc 3)
              (product cube 1 inc 3))

; Now, define factorial.
(define (factorial n)
  (product-iter identity 1 inc n))

(check-equal? (factorial 2) 2)
(check-equal? (factorial 4) 24)
(check-equal? (factorial 6) 720)

; Also, define pi-product.
(define (pi-product n)
  (define (term k)
    (/ (* 4 k (inc k))
       (square (inc (* 2 k)))))

  (product-iter term 1.0 inc n))

(check-= (* 4 (pi-product 1000)) 3.14159 1e-3)
