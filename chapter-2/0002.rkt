#lang sicp

; Exercise 2.2
;
; Consider the problem of representing line segments in a plane. Each segment is represented
; as a pair of points: a starting point and an ending point. Define a constructor make-segment
; and selectors start-segment and end-segment that define the representation of segments
; in terms of points. Furthermore, a point can be represented as a pair of numbers: the x coordinate
; and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and
; y-point that define this representation. Finally, using your selectors and constructors, define
; a procedure midpoint-segment that takes a line segment as argument and returns its midpoint
; (the point whose coordinates are the average of the coordinates of the endpoints). To try your
; procedures, you'll need a way to print points:
;
; (define (print-point p)
;   (newline)
;   (display "(")
;   (display (x-point p))
;   (display ",")
;   (display (y-point p))
;   (display ")"))

; Solution

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

(provide make-point x-point y-point)

(module* test #f
  (require rackunit)

  (define s1 (make-segment (make-point 0 0)
                           (make-point 2 2)))

  (check-equal? (midpoint-segment s1)
                (make-point 1 1))

  (define s2 (make-segment (make-point -1 -1)
                           (make-point 1 1)))

  (check-equal? (midpoint-segment s2)
                (make-point 0 0))

  (define s3 (make-segment (make-point -8 -3)
                           (make-point 2 5)))

  (check-equal? (midpoint-segment s3)
                (make-point -3 1)))
