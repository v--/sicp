#lang sicp

; Exercise 1.4
;
; Observe that our model of evaluation allows for combinations whose operators are compound expressions.
; Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Solution
;
; Basically, `b`'s value determines the operator to be used.
; The procedure could be more conventionally written as

(define (a-plus-abs-b-2 a b)
  (+ a (abs b)))

(check-equal? (a-plus-abs-b 1 +2) 3)
(check-equal? (a-plus-abs-b 1 -2) 3)
(check-equal? (a-plus-abs-b 1 -2) (a-plus-abs-b-2 1 -2))
