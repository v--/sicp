#lang sicp

(require (only-in chapter-2/0079 install-equ? equ?))

(require support/operation-table)
(require support/generic-number-package)
(require support/polynomial-package)

; Exercise 2.87
;
; Install =zero? for polynomials in the generic arithmetic package. This will allow adjoin-term to
; work for polynomials with coefficients that are themselves polynomials.

; Solution
; We want two polynomials to be equal if either:
;  * they are both empty
;  * they are both polynomials of the same variable and have identical terms
;
; The edge case of two empty polynomials being equal independently of their variable makes sense because:
;  * we compare polynomials syntactically and in this case they are both empty strings
;  * this allows us to implement =zero? using comparison to a zero polynomial, which is consistent
;    with the approach in exercise 2.79

(define (same-term? a b)
  (and (= (order a) (order b))
       (equ? (coeff a) (coeff b))))

(define (same-termlist? terms-a terms-b)
  (or (and (empty-termlist? terms-a) (empty-termlist? terms-b))
      (and (not (empty-termlist? terms-a))
           (not (empty-termlist? terms-b))
           (same-term? (first-term terms-a) (first-term terms-b))
           (same-termlist? (rest-terms terms-a) (rest-terms terms-b)))))

(define (install-polynomial-equ?)
  (put 'equ? '(polynomial polynomial)
       (lambda (p q)
         (or (and (empty-termlist? (term-list p)) (empty-termlist? (term-list q)))
             (and (eq? (variable p) (variable q))
                  (same-termlist? (term-list p) (term-list q))))))

  'done)

(define (install-polynomial-zero)
  (put 'zero 'polynomial
       (make-polynomial 'x the-empty-termlist))

  'done)

(provide install-polynomial-equ? install-polynomial-zero same-term?)

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-equ?))
  (void (install-polynomial-package))
  (void (install-polynomial-equ?))
  (void (install-polynomial-zero))

  ; Check equ?
  (check-true (equ? (make-polynomial 'x the-empty-termlist)
                    (make-polynomial 'x the-empty-termlist)))

  (check-true (equ? (make-polynomial 'x the-empty-termlist)
                    (make-polynomial 'y the-empty-termlist)))

  (check-true (equ? (make-polynomial 'x (list (make-term 3 3)))
                    (make-polynomial 'x (list (make-term 3 3)))))

  (check-false (equ? (make-polynomial 'x (list (make-term 3 2)))
                     (make-polynomial 'x (list (make-term 3 3)))))

  (check-false (equ? (make-polynomial 'x (list (make-term 2 3)))
                     (make-polynomial 'x (list (make-term 3 3)))))

  (check-false (equ? (make-polynomial 'x (list (make-term 3 3)))
                     (make-polynomial 'y (list (make-term 3 3)))))

  ; Check =zero?
  (check-true (=zero? (make-polynomial 'x the-empty-termlist)))
  (check-false (=zero? (make-polynomial 'x (list (make-term 3 3))))))
