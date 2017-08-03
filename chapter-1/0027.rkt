#lang sicp

; Exercise 1.27
;
; Demonstrate that the Carmichael numbers listed in footnote 47 really do fool the Fermat test.
; That is, write a procedure that takes an integer n and tests whether a^n is congruent to
; a modulo n for every a < n, and try your procedure on the given Carmichael numbers.

; Solution

(define (congruent a b modulo)
  (= (remainder a modulo) (remainder b modulo)))

(define (fermat-test n)
  (define (fermat-test-iter a)
    (cond [(= a n) true]
          [(congruent (expt a n) a n) (fermat-test-iter (+ a 1))]
          [else false]))

  (fermat-test-iter 0))

; Verify that the procedure works
(check-pred fermat-test 1)
(check-pred fermat-test 2)
(check-pred fermat-test 3)
(check-pred fermat-test 13)

(check-false (fermat-test 4))
(check-false (fermat-test 6))
(check-false (fermat-test 8))
(check-false (fermat-test 9))

; Check the given Carmichael numbers
(check-pred fermat-test 561)
(check-pred fermat-test 1105)
(check-pred fermat-test 1729)
(check-pred fermat-test 2465)
(check-pred fermat-test 2821)
(check-pred fermat-test 6601)
