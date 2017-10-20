#lang sicp

; Exercise 2.46
;
; A two-dimensional vector v running from the origin to a point can be represented as a pair
; consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors
; by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect.
; In terms of your selectors and constructor, implement procedures add-vect, sub-vect,
; and scale-vect that perform the operations vector addition, vector subtraction, and multiplying
; a vector by a scalar:
;   1) (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
;   2) (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2)
;   3) s * (x, y) = (s * x, s * y)

; Solution

; Vector implementation
(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

; Vector operations
(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b))
             (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect scalar vect)
  (make-vect (* scalar (xcor-vect vect))
             (* scalar (ycor-vect vect))))

(provide make-vect xcor-vect ycor-vect add-vect sub-vect scale-vect)

(module+ test
  (require rackunit)

  (check-equal? (add-vect (make-vect 0 1)
                          (make-vect 2 3))
                (make-vect 2 4))

  (check-equal? (sub-vect (make-vect 0 1)
                          (make-vect 2 3))
                (make-vect -2 -2))

  (check-equal? (scale-vect 2
                            (make-vect 1 3))
                (make-vect 2 6)))
