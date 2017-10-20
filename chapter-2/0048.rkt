#lang sicp

; Exercise 2.48
;
; A directed line segment in the plane can be represented as a pair of vectors - the vector
; running from the origin to the start-point of the segment, and the vector running from the origin
; to the end-point of the segment. Use your vector representation from exercise 2.46 to define
; a representation for segments with a constructor make-segment and selectors start-segment
; and end-segment.

; Solution

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(provide make-segment start-segment end-segment)

; Solution

(module+ test
  (require rackunit)
  (require (only-in chapter-2/0046 make-vect))

  (define start (make-vect 0 0))
  (define end (make-vect 1 1))
  (define segment (make-segment start end))

  (check-equal? (start-segment segment) start)
  (check-equal? (end-segment segment) end))
