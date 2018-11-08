#lang sicp

(require support/operation-table)
(require support/generic-number-package)

(require (only-in chapter-2/0079 equ?))

; Exercise 2.86
;
; Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and
; angles can be either ordinary numbers, rational numbers, or other numbers we might wish to add to
; the system. Describe and implement the changes to the system needed to accommodate this. You will
; have to define operations such as sine and cosine that are generic over ordinary numbers and
; rational numbers.

; Solution
; We will provide alternative magnitude and angle procedures for complex numbers in cartessian
; coordinates and alternative real-part and imag-part procedures for complex numbers in polar
; coordinates. This will require us to implement generic sin, cos and sqrt procedures.
;
; This way we will only be able to construct complex numbers using tagged numbers. We can use the
; heuristics from exercise 2.78 to construct complex numbers from built-in numbers directly.

(define (install-new-scheme-number-procedures)
  (put 'square-root '(scheme-number)
       (lambda (x)
         (make-scheme-number (sqrt x))))

  (put 'sine '(scheme-number)
       (lambda (x)
         (make-scheme-number (sin x))))

  (put 'cosine '(scheme-number)
       (lambda (x)
         (make-scheme-number (cos x))))

  (put 'arctangent '(scheme-number scheme-number)
       (lambda (y x)
         (make-scheme-number (atan y x))))
  'done)

(define (install-new-rational-procedures)
  (put 'square-root '(rational)
       (lambda (x)
         (make-rational (sqrt (numer x))
                        (sqrt (denom x)))))

  (put 'sine '(rational)
       (lambda (x)
         (make-rational (sin (/ (numer x)
                                (denom x)))
                        1)))

  (put 'cosine '(rational)
       (lambda (x)
         (make-rational (cos (/ (numer x)
                                (denom x)))
                        1)))

  (put 'arctangent '(rational rational)
       (lambda (y x)
         (make-rational (atan (/ (numer y)
                                 (denom y))
                              (/ (numer x)
                                 (denom x)))
                        1)))
  'done)

(define (install-modified-rectangular-procedures)
  (define real-part (get 'real-part '(rectangular)))
  (define imag-part (get 'imag-part '(rectangular)))

  (put 'magnitude '(rectangular)
       (lambda (z)
         (let ([real (real-part z)]
               [imag (imag-part z)])
           (square-root (add (mul real real)
                             (mul imag imag))))))

  (put 'angle '(rectangular)
       (lambda (z)
         (square-root (arctangent (real-part z)
                                  (imag-part z)))))

  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (attach-tag 'rectangular
                     (cons (mul r (cosine a))
                           (mul r (sine a))))))
  'done)

(define (install-modified-polar-procedures)
  (define magnitude (get 'magnitude '(polar)))
  (define angle (get 'angle '(polar)))

  (put 'real-part '(polar)
       (lambda (z)
         (mul (magnitude z) (cosine (angle z)))))

  (put 'real-part '(polar)
       (lambda (z)
         (mul (magnitude z) (sine (angle z)))))

  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (attach-tag 'polar
                     (cons (square-root (add (mul x x) (mul y y)))
                           (arctangent y x)))))

  'done)

(define (square-root number)
  (apply-generic 'square-root number))

(define (sine number)
  (apply-generic 'sine number))

(define (cosine number)
  (apply-generic 'cosine number))

(define (arctangent y x)
  (apply-generic 'arctangent y x))

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-new-scheme-number-procedures))
  (void (install-new-rational-procedures))
  (void (install-modified-rectangular-procedures))
  (void (install-modified-polar-procedures))

  (define z1 ((get 'make-from-real-imag 'rectangular)
              (make-scheme-number 1)
              (make-scheme-number 1)))

  (define z2 ((get 'make-from-mag-ang 'rectangular)
              (magnitude z1)
              (angle z1)))

  (check-true (equ? (magnitude z1)
                    (magnitude z2)))

  (define z3 ((get 'make-from-real-imag 'polar)
              (make-rational 1 2)
              (make-rational 1 2)))

  (define z4 ((get 'make-from-mag-ang 'polar)
              (magnitude z3)
              (angle z3)))

  (check-true (equ? (angle z3)
                    (angle z4))))
