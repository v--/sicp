#lang sicp

(require (only-in chapter-2/0007 make-interval lower-bound upper-bound))
(require (only-in chapter-2/0009 mul-interval))

; Exercise 2.11
;
; In passing, Ben also cryptically comments: "By testing the signs of the endpoints of the intervals,
; it is possible to break mul-interval into nine cases, only one of which requires more than two
; multiplications." Rewrite this procedure using Ben's suggestion.

; Solution

(define (mul-interval-cases x y)
  (let ([a (lower-bound x)]
        [b (upper-bound x)]
        [c (lower-bound y)]
        [d (upper-bound y)])

    (if (>= a 0)
        (if (>= c 0)
            (make-interval (* a c) (* b d))
            (if (>= d 0)
                (make-interval (* b c) (* b d))
                (make-interval (* b c) (* a d))))
        (if (>= c 0)
            (if (>= b 0)
                (make-interval (* a d) (* b d))
                (make-interval (* a d) (* b c)))
            (if (>= b 0)
                (if (>= d 0)
                    (make-interval (min (* a d) (* b c)) (* b d))
                    (make-interval (* b c) (* a c)))
                (if (>= d 0)
                    (make-interval (* a d) (* a c))
                    (make-interval (* b d) (* a c))))))))

(module+ test
  (require rackunit)

  (define-simple-check (check-impl-equal? x y)
    (equal? (mul-interval x y)
            (mul-interval-cases x y)))

  ; No negative endpoints
  (check-impl-equal? (make-interval  2  3) (make-interval  4  5)) ; 0 [2 3] (4 5)
  (check-impl-equal? (make-interval  2  4) (make-interval  3  5)) ; 0 [2 (3 4] 5)
  (check-impl-equal? (make-interval  2  5) (make-interval  3  4)) ; 0 [2 (3 4) 5]
  (check-impl-equal? (make-interval  3  5) (make-interval  2  4)) ; 0 (2 [3 4) 5]
  (check-impl-equal? (make-interval  4  5) (make-interval  2  3)) ; 0 (2 3) [4 5]

  ; One negative endpoint in the first interval
  (check-impl-equal? (make-interval -2  3) (make-interval  4  5)) ; [-2 0 3] (4 5)
  (check-impl-equal? (make-interval -2  4) (make-interval  3  5)) ; [-2 0 (3 4] 5)
  (check-impl-equal? (make-interval -2  5) (make-interval  3  4)) ; [-2 0 (3 4) 5]

  ; One negative endpoint in the second interval
  (check-impl-equal? (make-interval  3  5) (make-interval -4  2)) ; (-4 0 2) [3 5]
  (check-impl-equal? (make-interval  2  3) (make-interval -4  5)) ; (-4 0 [2 3] 5)
  (check-impl-equal? (make-interval  2  5) (make-interval -4  3)) ; (-4 0 [2 3) 5]

  ; Two negative endpoints
  (check-impl-equal? (make-interval -3 -2) (make-interval  4  5)) ; [-3 -2] 0 (4 5)
  (check-impl-equal? (make-interval -3  4) (make-interval -2  5)) ; [-3 (-2 0 4] 5)
  (check-impl-equal? (make-interval -2  5) (make-interval -3  4)) ; (-3 [-2 0 4) 5]
  (check-impl-equal? (make-interval  4  5) (make-interval -3 -2)) ; (-3 -2) 0 [4 5]

  ; One negative endpoint in the first interval and two in the second
  (check-impl-equal? (make-interval -2  5) (make-interval -4 -3)) ; (-4 -3) [-2 0 5]
  (check-impl-equal? (make-interval -4  5) (make-interval -3 -2)) ; [-4 (-3 -2) 0 5]
  (check-impl-equal? (make-interval -3  5) (make-interval -4 -2)) ; (-4 [-3 -2) 0 5]

  ; Two negative endpoints in the first interval and one in the second
  (check-impl-equal? (make-interval -4 -3) (make-interval -2  5)) ; [-4 -3] (-2 0 5)
  (check-impl-equal? (make-interval -3 -2) (make-interval -4  5)) ; (-4 [-3 -2] 0 5)
  (check-impl-equal? (make-interval -4 -2) (make-interval -3  5)) ; [-4 (-3 -2] 0 5)

  ; Four negative endpoints
  (check-impl-equal? (make-interval -5 -4) (make-interval -3 -2)) ; [-5 -4] (-3 -2) 0
  (check-impl-equal? (make-interval -5 -3) (make-interval -4 -2)) ; [-5 (-4 -3] -2) 0
  (check-impl-equal? (make-interval -5 -2) (make-interval -4 -3)) ; [-5 (-4 -3) -2] 0
  (check-impl-equal? (make-interval -4 -2) (make-interval -5 -3)) ; (-5 [-4 -3) -2] 0
  (check-impl-equal? (make-interval -3 -2) (make-interval -5 -4)) ; (-3 -2) [-5 -4] 0

  )
