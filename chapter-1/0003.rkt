#lang sicp

; Exercise 1.3
;
; Define a procedure that takes three numbers as arguments
; and returns the sum of the squares of the two larger numbers.

; Solution

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (frobnicate x y z)
  (sum-of-squares (max x y) (max (min x y) z)))

(module* test #f
  (require rackunit)

  (check-equal? (frobnicate 1 2 3) 13)
  (check-equal? (frobnicate 3 2 1) 13)
  (check-equal? (frobnicate 2 3 1) 13))
