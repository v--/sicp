#lang sicp

(require support/operation-table)
(require support/generic-number-package)

; Exercise 2.83
;
; Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in
; figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure
; that raises objects of that type one level in the tower. Show how to install a generic raise
; operation that will work for each type (except complex).

; Solution
; We will use the types [scheme-number, rational, complex] that already have implementations rather
; than [interger, rational, real, compex] in order to be able to actually write and test the code
; without being overwhelmed by the implementation.

(define (install-raise-procedures)
  (put 'raise '(scheme-number)
       (lambda (number) (make-rational number 1)))

  (put 'raise '(rational)
       (lambda (rational)
         (make-complex-from-real-imag
           (/ ((get 'numer '(rational)) rational)
              ((get 'denom '(rational)) rational))
           0)))

  'done)

(define (raise number)
  (apply-generic 'raise number))

(provide install-raise-procedures raise)

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-raise-procedures))

  (check-equal? (raise (make-scheme-number 13))
                (make-rational 13 1))

  (check-equal? (raise (make-rational 1 2))
                (make-complex-from-real-imag 1/2 0))

  (check-equal? (raise (raise (make-scheme-number 13)))
                (make-complex-from-real-imag 13 0)))
