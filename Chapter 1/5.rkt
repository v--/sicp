#lang sicp

; Exercise 1.5
;
; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using
; applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Then he evaluates the expression
;
; (test 0 (p))
;
; What behavior will Ben observe with an interpreter that uses applicative-order evaluation?
; What behavior will he observe with an interpreter that uses normal-order evaluation?
; Explain your answer.
;
; (Assume that the evaluation rule for the special form if is the same
; whether the interpreter is using normal or applicative order:
; The predicate expression is evaluated first, and the result determines
; whether to evaluate the consequent or the alternative expression.)

; Solution
;
; `p` is simply a procedure that evaluates itself and causes infinite recursion.
;
; Applicative-order evaluation of `(test 0 (p))` would also cause infinite recursion,
; since it will attempt to evaluate `(p)` before passing it to `test`, where it would have remained unevaluated.
;
; Normal-order evaluation, on the other hand, would simply evaluate zero, because the `test` procedure
; would be inlined and the "else" expression in the `if` form would never be evaluated.
