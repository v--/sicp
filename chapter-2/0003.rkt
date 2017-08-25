#lang sicp

(require (only-in chapter-2/0002 make-point x-point y-point))

; Exercise 2.3
;
; Implement a representation for rectangles in a plane. (Hint: You may want to make use
; of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute
; the perimeter and the area of a given rectangle. Now implement a different representation for
; rectangles. Can you design your system with suitable abstraction barriers, so that the same
; perimeter and area procedures will work using either representation?

; Solution
; NOTE: To avoid complexity, we assume that the rectangle sides are parallel to the coordinate axes.
; Define factories for the two required procedures
(define (perimeter-factory rectangle-width rectangle-height)
  (lambda (rectangle) (double (+ (rectangle-width rectangle)
                                 (rectangle-height rectangle)))))

(define (area-factory rectangle-width rectangle-height)
  (lambda (rectangle) (* (rectangle-width rectangle)
                         (rectangle-height rectangle))))

; The first implementation will only use a rectangle's diagonal
(define (make-rectangle-diagonals d1 d2)
  (cons d1 d2))

(define (rectangle-diagonals-width rectangle)
  (distance (x-point (car rectangle))
            (x-point (cdr rectangle))))

(define (rectangle-diagonals-height rectangle)
  (distance (y-point (car rectangle))
            (y-point (cdr rectangle))))

(define perimeter-diagonals (perimeter-factory rectangle-diagonals-width
                                               rectangle-diagonals-height))

(define area-diagonals (area-factory rectangle-diagonals-width
                                     rectangle-diagonals-height))

; The second implementation will store the origin (bottom-left) and the dimensions
(define (make-rectangle-dimensions origin dimensions)
  (cons origin dimensions))

(define (rectangle-dimensions-width rectangle)
  (x-point (cdr rectangle)))

(define (rectangle-dimensions-height rectangle)
  (y-point (cdr rectangle)))

(define perimeter-dimensions (perimeter-factory rectangle-dimensions-width
                                                rectangle-dimensions-height))

(define area-dimensions (area-factory rectangle-dimensions-width
                                      rectangle-dimensions-height))

(module+ test
  (require rackunit)

  (define r1-diagonals (make-rectangle-diagonals (make-point 0 0)
                                                 (make-point 2 2)))

  (define r1-dimensions (make-rectangle-diagonals (make-point 0 0)
                                                  (make-point 2 2)))

  (define r2-diagonals (make-rectangle-diagonals (make-point -8 -3)
                                                 (make-point 2 5)))

  (define r2-dimensions (make-rectangle-dimensions (make-point -8 -3)
                                                   (make-point 10 8)))

  ; Verify that the diagonals implementation works correctly
  (check-equal? (perimeter-diagonals r1-diagonals) 8)
  (check-equal? (area-diagonals r1-diagonals) 4)

  (check-equal? (perimeter-diagonals r2-diagonals) 36)
  (check-equal? (area-diagonals r2-diagonals) 80)

  ; Verify that the two implementations work identically
  (check-equal? (perimeter-diagonals r1-diagonals)
                (perimeter-dimensions r1-dimensions))

  (check-equal? (perimeter-diagonals r2-diagonals)
                (perimeter-dimensions r2-dimensions)))
