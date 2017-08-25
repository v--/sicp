#lang sicp

; Exercise 1.42
;
; Let f and g be two one-argument functions. The composition f after g is defined to be the function x  f(g(x)).
; Define a procedure compose that implements composition. For example, if inc is a procedure that adds 1 to its argument,
;
; ((compose square inc) 6)
; 49

; Solution

(define (compose f g)
  (lambda (x) (f (g x))))

(provide compose)

(module+ test
  (require rackunit)

  ; Verify that the procedure works as intended
  (check-equal? ((compose inc inc) 2) 4)
  (check-equal? ((compose inc dec) 2) 2)

  ; Verify that the procedure works as described in the exercise
  (check-equal? ((compose square inc) 6) 49))
