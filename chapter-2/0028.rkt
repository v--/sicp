#lang sicp

; Exercise 2.28
;
; Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))

(module+ test
  (require rackunit)

  (check-equal? (fringe x)
                (list 1 2 3 4))

  (check-equal? (fringe (list x x))
                (list 1 2 3 4 1 2 3 4)))

; Solution

(define (fringe items)
  (if (null? items)
      null
      (let ([item (car items)]
            [remainder (cdr items)])
        (append (if (list? item)
                    (fringe item)
                    (list item))
                (fringe remainder)))))

(provide fringe)
