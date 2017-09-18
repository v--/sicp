#lang sicp

(require (only-in chapter-2/0007 make-interval lower-bound upper-bound))
(require (only-in chapter-2/0008 sub-interval))

; Exercise 2.9
;
; The width of an interval is half of the difference between its upper and lower bounds.
; The width is a measure of the uncertainty of the number specified by the interval.
; For some arithmetic operations the width of the result of combining two intervals is a function
; only of the widths of the argument intervals, whereas for others the width of the combination
; is not a function of the widths of the argument intervals. Show that the width of the sum
; (or difference) of two intervals is a function only of the widths of the intervals being added
; (or subtracted). Give examples to show that this is not true for multiplication or division.

; Solution
; Original definitions

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y)))))

; Procedure for calculating interval widths
(define (interval-width interval)
  (halve (- (upper-bound interval)
            (lower-bound interval))))

(provide add-interval mul-interval div-interval interval-width)

(module+ test
  (require rackunit)

  (define i1 (make-interval 2 4))
  (define i2 (make-interval 2 6))
  (define i3 (make-interval 3 7))

  ; Verify that the width of the sum of two intervals equals the sum of the two widths.
  (check-equal? (interval-width (add-interval i1 i2))
                (+ (interval-width i1) (interval-width i2)))

  ; The same is true for subtraction.
  (check-equal? (interval-width (sub-interval i1 i2))
                (abs (- (interval-width i1) (interval-width i2))))

  ; The width of multiplied intervals cannot be meaningfully expressed as a function of
  ; the individual interval widths. To show this, here is a simple example of how
  ; multiplying intervals with pairwise equal widths produces intervals with different widths.
  (check-not-equal? (interval-width (mul-interval i1 i2))
                    (interval-width (mul-interval i1 i3)))

  ; The same is true for division
  (check-not-equal? (interval-width (div-interval i1 i2))
                    (interval-width (div-interval i1 i3))))
