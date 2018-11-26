#lang sicp

(require support/operation-table)
(require support/generic-number-package)

(require (only-in chapter-2/0079 install-equ? equ?))
(require (only-in chapter-2/0083 install-raise-procedures raise))
(require (only-in chapter-2/0084 apply-generic-upcast))

; Exercise 2.85.
;
; This section mentioned a method for "simplifying" a data object by lowering it in the tower of
; types as far as possible. Design a procedure drop that accomplishes this for the tower described
; in exercise 2.83. The key is to decide, in some general way, whether an object can be lowered.
; For example, the complex number 1.5 + 0i can be lowered as far as real, the complex number 1 + 0i
; can be lowered as far as integer, and the complex number 2 + 3i cannot be lowered at all. Here is
; plan for determining whether an object can be lowered: Begin by defining a generic operation
; project that ``pushes'' an object down in the tower. For example, projecting a complex number
; would involve throwing away the imaginary part. Then a number can be dropped if, when we project
; it and raise the result back to the type we started with, we end up with something equal to what
; we started with. Show how to implement this idea in detail, by writing a drop procedure that
; drops an object as far as possible. You will need to design the various projection operations and
; install project as a generic operation in the system. You will also need to make use of a generic
; equality predicate, such as described in exercise 2.79. Finally, use drop to rewrite apply-generic
; from exercise 2.84 so that it "simplifies" its answers.

; Solution
; As in exercise 2.83, we will use the implemented types [scheme-number, rational, complex] rather
; than [interger, rational, real, compex]. We provide projections from complex to rational and
; from rational to scheme "numbers", although these projections don't really make sense.

(define (install-project-procedures)
  (put 'project '(complex)
       (lambda (complex) (make-rational (real-part complex) 1)))

  (put 'project '(rational)
       (lambda (rational) (make-scheme-number (/
                                                (numer rational)
                                                (denom rational)))))

  'done)

(define (project number)
  (apply-generic 'project number))

(define (drop number)
  (if (eq? (type-tag number) 'scheme-number) ; The lowest possible type
      number
      (let ([projection (project number)])
        (if (equ? (raise projection) number)
            (drop projection)
            number))))

(define (apply-generic-with-drop op . args)
  (drop (apply apply-generic-upcast (cons op args))))

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-equ?))
  (void (install-raise-procedures))
  (void (install-project-procedures))

  ; Test project
  (check-equal? (project (make-complex-from-real-imag 1 1))
                (make-rational 1 1))

  (check-equal? (project (make-rational 1 2))
                (make-scheme-number 1/2))

  ; Test drop
  (check-equal? (drop (make-complex-from-real-imag 1 1))
                (make-complex-from-real-imag 1 1))

  (check-equal? (drop (make-rational 2 1))
                (make-scheme-number 2))

  (check-equal? (drop (make-complex-from-real-imag 1 0))
                (make-scheme-number 1))

  ; Test apply-generic-with-drop
  (check-equal? (apply-generic-with-drop 'add
                                         (make-complex-from-real-imag 1 2)
                                         (make-complex-from-real-imag 1 -2))
                (make-scheme-number 2)))
