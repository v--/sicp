#lang sicp

(require (only-in chapter-2/0007 make-interval lower-bound upper-bound))

; Exercise 2.8
;
; Using reasoning analogous to Alyssa's, describe how the difference of two intervals
; may be computed. Define a corresponding subtraction procedure, called sub-interval.

; Solution

(define (sub-interval x y)
  (let ([a (- (lower-bound x) (lower-bound y))]
        [b (- (upper-bound x) (upper-bound y))])
    (make-interval (min a b) (max a b))))

(provide sub-interval)

(module+ test
  (require rackunit)

  (define i1 (make-interval 15 20))
  (define i2 (make-interval 1 2))

  (check-equal? (sub-interval i1 i2)
                (make-interval 14 18))

  (check-equal? (sub-interval i2 i1)
                (make-interval -18 -14)))
