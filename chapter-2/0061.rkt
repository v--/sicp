#lang sicp

; Exercise 2.61
;
; Give an implementation of adjoin-set using the ordered representation. By analogy with
; element-of-set? show how to take advantage of the ordering to produce a procedure that requires
; on the average about half as many steps as with the unordered representation.

; Solution

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ([smallest (car set)])
        (cond [(< x smallest) (cons x set)]
              [(= x smallest) set]
              [else (cons smallest (adjoin-set x (cdr set)))]))))

(provide adjoin-set)

(module+ test
  (require rackunit)

  (check-equal? (adjoin-set 1 null)
                '(1))

  (check-equal? (adjoin-set 1 '(1 2 3))
                '(1 2 3))

  (check-equal? (adjoin-set 1 '(2 3))
                '(1 2 3))

  (check-equal? (adjoin-set 2 '(1 3))
                '(1 2 3)))
