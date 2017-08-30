#lang sicp

; Exercise 2.7
;
; Alyssa's program is incomplete because she has not specified the implementation
; of the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

; Define selectors upper-bound and lower-bound to complete the implementation.

; Solution

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(provide make-interval lower-bound upper-bound)

(module+ test
  (require rackunit)

  (check-equal? (lower-bound (make-interval -2 3)) -2)
  (check-equal? (upper-bound (make-interval -2 3)) 3))
