#lang sicp

(require (only-in chapter-2/0009 mul-interval))
(require (only-in chapter-2/0012 make-center-percent center percent))

; Exercise 2.13
;
; Show that under the assumption of small percentage tolerances there is a simple formula
; for the approximate percentage tolerance of the product of two intervals in terms
; of the tolerances of the factors. You may simplify the problem by assuming that all numbers
; are positive.

; Solution
; We saw is exercise 2.11 that the product of two intervals with positive endpoints is
;   [l_x, u_x] × [l_y, u_y] = [l_x * l_y, u_x * u_y],
; where x and y are intervals and l_x and u_x are the lower and upper bounds of x.
;
; Constructing an interval using the make-center-percent procedure from 2.12 can be rewritten
; in mathematical notation as
;   x = [c * (1 - p_x / 100), c * (1 + p_x / 100)].

; Thus, multiplying two center-percent intervals with positive endpoints results in
;   z = [c_x * c_y * (1 - p_x / 100) * (1 - p_y / 100), c_x * c_y * (1 + p_x / 100) * (1 + p_y / 100)].
;
; Dividing both sides of the interval by c_x * c_y, we obtain
;   (1 - p_x / 100) * (1 - p_y / 100) = 1 - (p_x + p_y - p_x * p_y / 100) / 100
; and
;   (1 + p_x / 100) * (1 + p_y / 100) = 1 + (p_x + p_y + p_x * p_y / 100) / 100
; correspondingly, which means that the percentage of z can be approximated by p_x + p_y with
; a tolerance of p_x * p_y / 100.

(module+ test
  (require rackunit)

  ; Verify that the percentage is within p_x * p_y / 100 of p_x + p_y.
  (check-= (percent (mul-interval (make-center-percent 100 10) (make-center-percent 100 15))) 25 1.5)

  ; The approximation is close enough to the real product percentage if the factor percentages are small enough.
  (check-= (percent (mul-interval (make-center-percent 100 1) (make-center-percent 100 2))) 3 default-tolerance))
