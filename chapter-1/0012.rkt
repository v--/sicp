#lang sicp

; Exercise 1.12
;
; The following pattern of numbers is called Pascal's triangle.
;
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;
; The numbers at the edge of the triangle are all 1, and each number inside the triangle
; is the sum of the two numbers above it. Write a procedure that computes elements
; of Pascal's triangle by means of a recursive process.

; Solution

(define (triangle-item row col)
  (if (or (= row 0) (= col 0))
      1
      (+ (triangle-item (- row 1) col) (triangle-item row (- col 1)))))

(check-equal? (triangle-item 0 0) 1)
(check-equal? (triangle-item 0 10) 1)
(check-equal? (triangle-item 10 0) 1)

(check-equal? (triangle-item 1 1) 2)
(check-equal? (triangle-item 2 2) 6)
(check-equal? (triangle-item 3 3) 20)
