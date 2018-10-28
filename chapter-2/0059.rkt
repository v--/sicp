#lang sicp

; Exercise 2.59
;
; Implement the union-set operation for the unordered-list representation of sets.

; Solution
; Original definitions

(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) null]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

; union-set

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (union-set (adjoin-set (car set2) set1) (cdr set2))))

(provide element-of-set?)

(module+ test
  (require rackunit)

  (define a '(1 2 3))
  (define b '(2 3 4))
  (check-equal? (sort (union-set a b) <=) '(1 2 3 4)))
