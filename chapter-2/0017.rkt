#lang sicp

; Exercise 2.17
;
; Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

(module+ test
  (require rackunit)

  (check-equal? (last-pair (list 23 72 149 34))
               (list 34)))

; Solution

(define (last-pair items)
  (when (null? items)
    (error "The list is already null"))

  (let ([next (cdr items)])
    (if (null? next)
        items
        (last-pair next))))
