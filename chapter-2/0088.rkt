#lang sicp

(require (only-in chapter-2/0079 install-equ? equ?))
(require (only-in chapter-2/0087 install-polynomial-equ? install-polynomial-zero))

(require support/operation-table)
(require support/generic-number-package)
(require support/polynomial-package)

; Exercise 2.87
;
; Extend the polynomial system to include subtraction of polynomials. (Hint: You may find it helpful
; to define a generic negation operation.)

; Solution

(put 'additive-inverse 'scheme-number
     (lambda (x) (- x)))

(define (term-additive-inverse term)
  (make-term
    (order term)
    (additive-inverse (coeff term))))

(define (term-list-additive-inverse lst)
  (if (null? lst)
      lst
      (cons (term-additive-inverse (first-term lst))
            (term-list-additive-inverse (rest-terms lst)))))

(put 'additive-inverse 'polynomial
     (lambda (p)
       (make-polynomial (variable p)
                        (term-list-additive-inverse (term-list p)))))

(define (additive-inverse x)
  ((get 'additive-inverse (type-tag x)) (contents x)))

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-equ?))
  (void (install-polynomial-package))
  (void (install-polynomial-zero))
  (void (install-polynomial-equ?))

  (check equ?
         (additive-inverse (make-polynomial 'x the-empty-termlist))
         (make-polynomial 'x the-empty-termlist))

  (check equ?
         (additive-inverse (make-polynomial 'x (list (make-term 3 2))))
         (make-polynomial 'x (list (make-term 3 -2)))))
