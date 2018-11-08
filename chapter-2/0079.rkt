#lang sicp

(require support/operation-table)
(require support/generic-number-package)

; Exercise 2.79
;
; Define a generic equality predicate equ? that tests the equality of two numbers, and install it in
; the generic arithmetic package. This operation should work for ordinary numbers, rational numbers,
; and complex numbers.

; Solution
(put 'equ? '(scheme-number scheme-number)
     (lambda (a b) (= a b)))

(put 'equ? '(rational rational)
     (lambda (a b)
       (define (numer x) (car x))
       (define (denom x) (cdr x))
       (and (= (numer a) (numer b))
            (= (denom a) (denom b)))))

; This procedure can of course be implemented separately for the different representations, but the
; gain from this is neglectable.
(put 'equ? '(complex complex)
     (lambda (a b)
       (and (= (real-part a) (real-part b))
            (= (imag-part a) (imag-part b)))))

; Use the obvious conversion from built-in numbers to rational numbers
(put 'equ? '(scheme-number rational)
     (lambda (a b) ((get 'equ? '(rational rational)) (contents (make-rational a 1)) b)))

(put 'equ? '(rational scheme-number)
     (lambda (a b) ((get 'equ? '(scheme-number rational)) b a)))

(define (equ? a b)
  (apply-generic 'equ? a b))

(provide equ?)

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))

  ; Built-in numbers
  (check-true (equ? (make-scheme-number 1)
                    (make-scheme-number 1)))

  (check-false (equ? (make-scheme-number 1)
                     (make-scheme-number 2)))

  ; Rational numbers
  (check-true (equ? (make-rational 1 2)
                    (make-rational 1 2)))

  (check-true (equ? (make-rational 1 2)
                    (make-rational 2 4)))

  (check-false (equ? (make-rational 1 2)
                     (make-rational 2 1)))

  ; Complex numbers
  (check-true (equ? (make-complex-from-real-imag 1 1)
                    (make-complex-from-real-imag 1 1)))

  (check-false (equ? (make-complex-from-real-imag 0 0)
                     (make-complex-from-real-imag 1 1)))

  ; A scheme number and a rational number and the other way around
  (check-true (equ? (make-scheme-number 2)
                    (make-rational 2 1)))

  (check-true (equ? (make-rational 2 1)
                    (make-scheme-number 2)))

  (check-false (equ? (make-rational 1 2)
                     (make-scheme-number 2))))
