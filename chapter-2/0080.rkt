#lang sicp

(require support/operation-table)
(require support/generic-number-package)

(require (only-in chapter-2/0079 install-equ? equ?))

; Exercise 2.80
;
 ; Define a generic predicate =zero? that tests if its argument is zero, and install it in the
 ; generic arithmetic package. This operation should work for ordinary numbers, rational numbers,
 ; and complex numbers.

; Solution
; We define zeros for the different number types and then use equ? from exercise 2.79 to compare
; the number with the zero of the corresponding type.

; Define an installation procedure to handle the late binding of the constructors
(define (install-zeros)
  (put 'zero 'scheme-number
       (make-scheme-number 0))

  (put 'zero 'rational
       (make-rational 0 1))

  (put 'zero 'complex
       (make-complex-from-real-imag 0 0))

  'done)

(define (=zero? number)
  (equ? (get 'zero (type-tag number)) number))

(provide =zero? install-zeros)

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-equ?))
  (void (install-zeros))

  ; Built-in numbers
  (check-true (=zero? (make-scheme-number 0)))
  (check-false (=zero? (make-scheme-number 1)))

  ; Rational numbers
  (check-true (=zero? (make-rational 0 1)))
  (check-true (=zero? (make-rational 0 2)))
  (check-false (=zero? (make-rational 1 2)))

  ; Rational numbers
  (check-true (=zero? (make-complex-from-real-imag 0 0)))
  (check-true (=zero? (make-complex-from-mag-ang 0 0)))
  (check-false (=zero? (make-complex-from-real-imag 0 1))))
