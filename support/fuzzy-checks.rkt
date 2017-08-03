#lang sicp

(define (fuzzy-equals? a b [tolerance tolerance])
  (< (abs (- a b)) tolerance))

(define (fuzzy-ratio-equals? a b [tolerance tolerance])
  (< (abs (- (/ a b) 1))
     tolerance))

(define-simple-check (check/= a b tolerance)
  (fuzzy-ratio-equals? a b tolerance))

(provide fuzzy-equals?)
(provide fuzzy-ratio-equals?)
(provide check/=)