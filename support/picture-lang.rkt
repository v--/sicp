#lang sicp

(require math/matrix)
(require rackunit)

(require support/fuzzy-checks)
(require (only-in chapter-2/0046 make-vect xcor-vect ycor-vect add-vect sub-vect scale-vect))
(require (only-in chapter-2/0047 make-frame origin-frame edge1-frame edge2-frame))
(require (only-in chapter-2/0048 make-segment start-segment end-segment))

; To keep things simple and testable, I have tweaked the original procedures to produce
; boolean matrices instead of directly drawing on the screen. Each matrix is a representation
; of a monochrome image. matrix-or is used to overlay two drawings (matrices).
; The matrix->string procedure is then used to make human-readable strings out of these matrices.

; Original definitions
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ([m (frame-coord-map frame)])
      (let ([new-origin (m origin)])
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for/fold ([result (make-matrix canvas-size canvas-size false)])
      ([segment segment-list])
      (matrix-or result
                 (draw-line
                   ((frame-coord-map frame) (start-segment segment))
                   ((frame-coord-map frame) (end-segment segment)))))))

; A matrix size of 17 is just big enough to render everything that is required and small enough
; to keep things fast and the pictures small.
(define canvas-size 17)

; Additional matrix utilities
(define (matrix-or a b)
  (for/matrix canvas-size
              canvas-size
              ([a-item (matrix->list a)]
               [b-item (matrix->list b)])
              (or a-item b-item)))

(define (matrix->string matrix)
  (string-join (map
                 (lambda (row)
                   (string-join (map
                                  (lambda (col) (if col "#" " "))
                                  row)
                                ""))
                 (matrix->list* matrix))
               "\n"))

; Drawing helpers
(define (lerp c1-start c1-end c2-start c2-end value)
  (+ (* (/ (- c2-end c2-start)
           (- c1-end c1-start))
        (- value c1-start))
     c2-start))

(define (draw-line start end)
  (let ([x-start (xcor-vect start)]
        [y-start (ycor-vect start)]
        [x-end (xcor-vect end)]
        [y-end (ycor-vect end)])
    (define (lerp-x value)
      (lerp y-start y-end x-start x-end value))

    (define (lerp-y value)
      (lerp x-start x-end y-start y-end value))

    (define fill?
      (if (<= (abs (- y-end y-start)) (abs (- x-end x-start)))
          (lambda (x y) (fuzzy-equal? y (lerp-y x) 0.75))
          (lambda (x y) (fuzzy-equal? x (lerp-x y) 0.75))))

    (let ([x-min (min x-start x-end)]
          [x-max (max x-start x-end)]
          [y-min (min y-start y-end)]
          [y-max (max y-start y-end)])
      (build-matrix canvas-size
                    canvas-size
                    (lambda (x y)
                      (and (<= x-min x x-max)
                           (<= y-min y y-max)
                           (fill? x y)))))))

(define default-frame (make-frame (make-vect 0 0)
                                  (make-vect 0 (dec canvas-size))
                                  (make-vect (dec canvas-size) 0)))

(provide transform-painter segments->painter matrix-or matrix->string default-frame)
(provide (all-from-out chapter-2/0046))
(provide (all-from-out chapter-2/0047))
(provide (all-from-out chapter-2/0048))
