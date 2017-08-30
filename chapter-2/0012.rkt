#lang sicp

(require (only-in chapter-2/0007 make-interval lower-bound upper-bound))

; Exercise 2.12
;
; Define a constructor make-center-percent that takes a center and a percentage tolerance
; and produces the desired interval. You must also define a selector percent that produces
; the percentage tolerance for a given interval. The center selector is the same as the one
; shown above.

; Solution
; Original definitions

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Desired constructor and selector

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent interval)
  (* 100
     (/ (width interval)
        (center interval))))

(provide make-center-percent width center percent)

(module+ test
  (require rackunit)

  (define (verify c p desired-interval)
    (check-equal? (make-center-percent c p) desired-interval)
    (check-equal? (center desired-interval) c)
    (check-equal? (percent desired-interval) p))

  (verify 10 50 (make-interval 5 15))
  (verify 12 25 (make-interval 9 15))
  (verify 12 250 (make-interval -18 42))
  (verify 100 33 (make-interval 67 133)))
