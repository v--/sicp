#lang sicp

; Exercise 1.21
; Use the smallest-divisor procedure to find the smallest divisor
; of each of the following numbers: 199, 1999, 19999.

; Solution
; Original definitions

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))

; Verify that 199 and 1999 are prime, while 19999 is not.
(check-equal? (smallest-divisor 199) 199)
(check-equal? (smallest-divisor 1999) 1999)
(check-equal? (smallest-divisor 19999) 7)

(provide smallest-divisor)
