#lang sicp

; Exercise 2.62
;
; Give a Theta(n) implementation of union-set for sets represented as ordered lists.

; Solution

(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(> (car set1) (car set2)) (union-set set2 set1)]
        [(= (car set1) (car set2)) (cons (car set1)
                                         (union-set (cdr set1) (cdr set2)))]
        [else (cons (car set1)
                    (union-set (cdr set1) set2))]
        ))

(provide (rename-out [union-set union-set-ordered]))

(module+ test
  (require rackunit)

  (check-equal? (union-set null '(1 2 3 4))
                '(1 2 3 4))

  (check-equal? (union-set '(1 2) '(1 2 3 4))
                '(1 2 3 4))

  (check-equal? (union-set '(1 2) '(3 4))
                '(1 2 3 4))

  (check-equal? (union-set '(2 3) '(1 4))
                '(1 2 3 4)))
