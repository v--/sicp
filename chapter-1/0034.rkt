#lang sicp

; Exercise 1.34
;
; Suppose we define the procedure

(define (f g)
  (g 2))

; Then we have

(module+ test
  (require rackunit)

  (check-equal? (f square)
                4)

  (check-equal? (f (lambda (z) (* z (+ z 1))))
                6))

; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

; Solution
; The application of (f f) will yield
;   (f f),
;   (f 2),
;   (2 2),
; and an error will be raised when the interpreter tries to apply 2.
