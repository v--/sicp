#lang sicp

(require (only-in chapter-1/0043 repeated))

; Exercise 1.44
;
; The idea of smoothing a function is an important concept in signal processing.
; If f is a function and dx is some small number, then the smoothed version of f is the function
; whose value at a point x is the average of f(x - dx), f(x), and f(x + dx).
; Write a procedure smooth that takes as input a procedure that computes f and returns a procedure
; that computes the smoothed f. It is sometimes valuable to repeatedly smooth a function (that is,
; smooth the smoothed function, and so on) to obtained the n-fold smoothed function. Show how to
; generate the n-fold smoothed function of any given function using smooth and repeated from
; exercise 1.43.

; Solution

(define dx default-tolerance)

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(module* test #f
  (require rackunit)

  (define (discontinuous x)
    (if (integer? x) 1 0))

  ; Verify that repeated applications of smooth diminish the discontinuities in the above procedure.

  (check-= ((smooth discontinuous) 1.0)
           0.3
           0.05)

  (check-= (((repeated smooth 3) discontinuous) 1.0)
           0.1
           0.05)

  (check-= (((repeated smooth 10) discontinuous) 1.0)
           0
           0.05))
