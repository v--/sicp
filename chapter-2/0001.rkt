#lang sicp

; Exercise 2.1
;
; Define a better version of make-rat that handles both positive and negative arguments.
; make-rat should normalize the sign so that if the rational number is positive, both the numerator
; and denominator are positive, and if the rational number is negative, only the numerator is negative.

; Solution

(define (make-rat n d)
  (let ([g (gcd n d)]
        [abs-n (abs n)]
        [abs-d (abs d)])

    (cons (/ (* n (/ abs-d d)) g)
          (/ abs-d g))))

(module+ test
  (require rackunit)

  (check-equal? (make-rat 10 2)
                (cons 5 1))

  (check-equal? (make-rat -10 2)
                (cons -5 1))

  (check-equal? (make-rat 10 -2)
                (cons -5 1))

  (check-equal? (make-rat -10 -2)
                (cons 5 1)))
