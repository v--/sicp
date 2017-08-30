#lang sicp

(require (only-in chapter-2/0007 make-interval))
(require (only-in chapter-2/0012 make-center-percent percent))
(require (only-in chapter-2/0014 par1 par2))

; Exercise 2.15
;
; Eva Lu Ator, another user, has also noticed the different intervals computed by different but
; algebraically equivalent expressions. She says that a formula to compute with intervals using
; Alyssa's system will produce tighter error bounds if it can be written in such a form that
; no variable that represents an uncertain number is repeated. Thus, she says, par2 is a "better"
; program for parallel resistances than par1. Is she right? Why?

; Solution
;
; We will prove the desired result only for intervals with positive endpoints. First, however, we
; need the following lemma.
;
; Lemma:
;   Let x be an interval. The percentage of y = 1 / x, the interval's multiplicative inverse,
;   equals p(x).
;
; Proof:
;   p(y) = 100 * | (u_y - l_y) / (u_y + l_y) |
;        = 100 * | (1 / l_x - 1 / u_x) / (1 / l_x + 1 / u_x) |
;        = 100 * | (u_x - l_x) / (u_x + l_x) | = p(x).
; QED
;
; Theorem:
;   par2 produces results with a smaller percentage variance than par1.
;
; Proof:
;   Let X = (R1 * R2) / (R1 + R2) and Y = 1 / (1 / R1 + 1 / R2). These intervals correspond to
;   par1 and par2, respectively.
;
;   The percentage for Y equals
;     p(Y) = p(1 / (1 / R1 + 1 / R2)) = p(1 / R1 + 1 / R2) = p(R1 + R2)
;
;   The percentage for X equals
;     p(X) = p(R1 * R2 / (R1 + R2)) = p(R1 * R2 * (R1 + R2)),
;   which can be approximated by (the approximation is derived in from exercise 2.13)
;     p(R1 * R2) + p(R1 + R2) = P(Y) + p(R1 * R2).
;
;   We showed that p(Y) is, in general, smaller than p(X), which is the desired result.
; QED

(module+ test
  (require rackunit)

  (define i1 (make-center-percent 109 10))
  (define i2 (make-center-percent 197 10))

  (check >
         (percent (par1 i1 i2))
         (percent (par2 i1 i2))))
