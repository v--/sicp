#lang sicp

; Exercise 2.26
;
; Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

; What result is printed by the interpreter in response to evaluating each of the following expressions:

; Solution

(module+ test
  (require rackunit)

  (check-equal? (append x y)
                (list 1 2 3 4 5 6))

  (check-equal? (cons x y)
                (list (list 1 2 3) 4 5 6))

  (check-equal? (list x y)
                (list (list 1 2 3) (list 4 5 6))))
