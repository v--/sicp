#lang sicp

(require (only-in chapter-2/0007 make-interval lower-bound upper-bound))
(require (only-in chapter-2/0009 div-interval interval-width))

; Exercise 2.10
;
; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments
; that it is not clear what it means to divide by an interval that spans zero.
; Modify Alyssa's code to check for this condition and to signal an error if it occurs.

; Solution

(define (fortified-div-interval x y)
  (if (<= (lower-bound y) 0 (upper-bound y))
      (error "Cannot divide by an interval that spans zero")
      (div-interval x y)))

(module+ test
  (require rackunit)

  (define i1 (make-interval 2 4))
  (define i2 (make-interval -1 1))

  (check-exn exn:fail?
             (lambda () (fortified-div-interval i1 i2))
             "Cannot divide by an interval that spans zero"))
