#lang sicp

(require (only-in chapter-1/0042 compose))

; Exercise 1.43
;
; If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f,
; which is defined to be the function whose value at x is f(f(...(f(x))...)). For example, if f is the function
; x -> x + 1, then the nth repeated application of f is the function x -> x + n. If f is the operation
; of squaring a number, then the nth repeated application of f is the function that raises its argument to the
; (2^n)th power. Write a procedure that takes as inputs a procedure that computes f and a positive integer n
; and returns the procedure that computes the nth repeated application of f.
; Your procedure should be able to be used as follows:
;
; ((repeated square 2) 5)
; 625

; Solution

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(provide repeated)

(module+ test
  (require rackunit)

  ; Verify that the procedure works as described in the exercise
  (check-equal? ((repeated inc 10) 5)
                15)

  (check-equal? ((repeated square 10) 5)
                (expt 5 (expt 2 10)))

  (check-equal? ((repeated square 2) 5)
                625))
