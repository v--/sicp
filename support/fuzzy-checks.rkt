#lang sicp

(require (only-in rackunit define-simple-check))

(define (fuzzy-equal? a b [tolerance default-tolerance])
  (< (abs (- a b)) tolerance))

(define (fuzzy-ratio-equal? a b [tolerance default-tolerance])
  (< (abs (- (/ a b) 1))
     tolerance))

(define-simple-check (check/= a b tolerance)
  (fuzzy-ratio-equal? a b tolerance))

(provide fuzzy-equal?)
(provide fuzzy-ratio-equal?)
(provide check/=)
