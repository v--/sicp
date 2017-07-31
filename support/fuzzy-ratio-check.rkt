#lang racket

(#%require rackunit)

(define (fuzzy-ratio-equals? a b tolerance)
  (< (abs (- (/ a b) 1))
     tolerance))

(define-simple-check (check/= a b tolerance)
                     (fuzzy-ratio-equals? a b tolerance))

(provide fuzzy-ratio-equals?)
(provide check/=)
