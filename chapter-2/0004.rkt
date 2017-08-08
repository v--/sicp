#lang sicp

; Exercise 2.4
;
; Here is an alternative procedural representation of pairs. For this representation,
; verify that (car (cons x y)) yields x for any objects x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; What is the corresponding definition of cdr?

; Solution

(define (cdr z)
  (z (lambda (p q) q)))

(module* test #f
  (require rackunit)

  (define x 4)
  (define y 13)

  (define-simple-check (check-x? a)
    (= a x))

  ; Use substitution to inspect how car works
  (check-x? (car (cons x y)))
  (check-x? (car (lambda (m) (m x y))))
  (check-x? ((lambda (m) (m x y)) (lambda (p q) p)))
  (check-x? x)

  ; Verify that cdr works
  (check-equal? (cdr (cons x y)) y))
