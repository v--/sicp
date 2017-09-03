#lang sicp

; Exercise 2.18
;
; Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

(module+ test
  (require rackunit)

  (check-equal? (reverse (list 1 4 9 16 25))
                (list 25 16 9 4 1)))

; Solution

(define (reverse items)
  (define (iter items reversed)
    (if (null? items)
        reversed
        (iter (cdr items) (cons (car items) reversed))))

  (iter items null))
