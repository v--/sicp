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
; We can define interval operations that do not make much sense in practice, but satisfy
; all field axioms and thus let expressions like par1 and par2 produce identical results.
; We must, however, assume that intervals where the lower bound is greater or equal to the
; upper bound are considered meaningful (this allows the mul-interval procedure to be simple to
; implement and reason about).
;
; The add-interval and mul-interval operations defined in the book satisfy all field axioms
; with two exceptions:
;   * No multiplicative inverses exist for intervals with at least one zero endpoint.
;     The following procedure creates multiplicative inverses for all non-zero intervals
;     in a similar fashion to the original implementation (embedded into div-interval).
;     There are two differences:
;       * The result endpoints are swapped and thus the lower bound will most likely be greater
;         than the upper bound.
;       * A zero endpoint in the source interval is preserved in the destination interval.

(define (mul-inverse x)
  (when (equal? x zero)
    (error "The zero element does not have a multiplicative inverse"))

  (define (helper a)
    (if (= a 0) 0 (/ 1 a)))

  (make-interval (helper (lower-bound x))
                 (helper (upper-bound x))))

;   * Multiplication is not distributive over addition.
;     This can be fixed with the following mul-interval implementation:

(define (mul-interval x y)
  (define (helper a b)
    (if (= a b 0) 1 (* a b)))

  (make-interval (helper (lower-bound x) (lower-bound y))
                 (helper (upper-bound x) (upper-bound y))))

;     The augmentation that is achieved using the helper makes sure that an interval that
;     contains one zero endpoint produces the identity when it is multiplied by it's inverse.
;       e.g. [0, 7] * [0, 1/7] = [1, 1]
;
; We need some auxiliary definitions - the additive inverse procedure, the additive identity (zero)
; and the multiplicative identity (one).

(define (add-inverse x)
  (make-interval (- (lower-bound x))
                 (- (upper-bound x))))

(define zero (make-interval 0 0))
(define one (make-interval 1 1))

; We can now show that all field axioms are fulfilled (without including proofs as they are trivial):
;   * add-interval forms an abelian group with zero as an identity element and add-inverse producing
; inverse intervals.
;   * mul-interval inherits it's commutative, associative and distributive properties from
; real number multiplication and thus also forms an abelian group.
;   * mul-interval distributes over add-interval.
;
; In order to complete a meaningful interval field implementation, we just need to redefine
; subtraction and division as is conventional for fields:

(define (sub-interval x y)
  (add-interval x (add-inverse y)))

(define (div-interval x y)
  (mul-interval x (mul-inverse y)))

; We can now show that par1 and par2 from exercise 2.14 produce equal results with the operations
; defined above.

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
