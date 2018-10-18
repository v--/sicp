#lang sicp

(require support/operation-table)

; Exercise 2.75
;
; Implement the constructor make-from-mag-ang in message-passing style. This procedure should be
; analogous to the make-from-real-imag procedure given above.

; Solution
; Original definitions
(define (apply-generic-message-passing op arg) (arg op))

; Required procedure
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* mag (cos ang))]
          [(eq? op 'imag-part) (* mag (sin ang))]
          [(eq? op 'magnitude) mag]
          [(eq? op 'angle) ang]
          [else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op)]))
  dispatch)

(module+ test
  (require rackunit)

  (define z (make-from-mag-ang (sqrt 2) (/ pi 4)))

  (check-= (apply-generic-message-passing 'real-part z)
           1
           default-tolerance)

  (check-= (apply-generic-message-passing 'imag-part z)
           1
           default-tolerance)

  (check-= (apply-generic-message-passing 'magnitude z)
           (sqrt 2)
           default-tolerance)

  (check-= (apply-generic-message-passing 'angle z)
           (/ pi 4)
           default-tolerance))
