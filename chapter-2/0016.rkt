#lang sicp

(require (only-in chapter-2/0007 make-interval lower-bound upper-bound))
(require (only-in chapter-2/0009 add-interval interval-width))

; Exercise 2.16
;
; Explain, in general, why equivalent algebraic expressions may lead to different answers.
; Can you devise an interval-arithmetic package that does not have this shortcoming,
; or is this task impossible?

; We cannot reasonably expect equivalence between algebraic expressions without knowing that the
; underlying algebraic operations behave in accordance with some conventional rules. Most of these
; rules are derived from real number arithmetic, which is generalized by field structures.
;
; We can define interval operations that may not have much practical sense, but satisfy
; all field axioms and thus let par1 and par2 produce identical results. We must, however,
; assume that intervals where the lower bound is greater or equal to the upper bound are
; considered meaningful.
;
; The add-interval and mul-interval operations defined in the book satisfy all field axioms
; with two exceptions:
;   * Distributivity of multiplication of addition
;     This can be fixed by using the simplest possible multiplication implementation:

(define (mul-interval x y)
  (make-interval (* (lower-bound x) (lower-bound y))
                 (* (upper-bound x) (upper-bound y))))

;     This operation inherits it's commutative, associative and distributive properties from
;     real number multiplication.
;
;   * Existence of multiplicative inverses for all field elements except the zero element. We can
;     use the following dead-simple implementation, which creates multiplicative inverses for all
;     elements and is consistent with the mul-interval procedure above.

(define (mul-inverse x)
  (when (equal? x zero)
    (error "The zero element does not have a multiplicative inverse"))

  (define (helper a)
    (if (= a 0) 0 (/ 1 a)))

  (make-interval (helper (lower-bound x))
                 (helper (upper-bound x))))

; We must also redefine subtraction and division as is conventional for fields:

(define (add-inverse x)
  (make-interval (- (lower-bound x))
                 (- (upper-bound x))))

(define (sub-interval x y)
  (add-interval x (add-inverse y)))

(define (div-interval x y)
  (mul-interval x (mul-inverse y)))

; Finally, we define the additive identity, zero, and the multiplicative identity, one.

(define zero (make-interval 0 0))
(define one (make-interval 1 1))

; We can now show that par1 and par2 from exercise 2.14 produce equal results.

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (div-interval one
                (add-interval (div-interval one r1)
                              (div-interval one r2))))

(module+ test
  (require rackunit)

  (define i1 (make-interval 2 3))
  (define i2 (make-interval 5 7))

  (check-equal? (par1 i1 i2) (par2 i1 i2)))
