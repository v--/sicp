#lang sicp

(require (only-in chapter-2/0007 make-interval))
(require (only-in chapter-2/0009 add-interval mul-interval div-interval))

; Exercise 2.14
;
; Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions.
; Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight
; by using intervals whose width is a small percentage of the center value.
; Examine the results of the computation in center-percent form (see exercise 2.12).

; Solution
; Original definitions

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ([one (make-interval 1 1)])
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(provide par1 par2)

(module+ test
  (require rackunit)

  ; First, we verify that par1 and par2 can indeed produce different results

  (define i1 (make-interval 2 3))
  (define i2 (make-interval 5 7))
  (check-not-equal? (par1 i1 i2) (par2 i1 i2))

  ; The reason for this is that the equivalence between the formulas
  ;   (R1 * R2) / (R1 + R2)
  ; and
  ;   1 / (1 / R1 + 1 / R2)
  ; depends on the fact that division for real numbers is defined as the multiplication of the LHS
  ; by the multiplicative inverse of the RHS. In order for interval division to behave consistently
  ; with real number division, dividing an interval by itself should produce the unit interval.
  ; Unfortunately, this is not the case.

  (check-not-equal? (div-interval i1 i1) (make-interval 1 1)))
