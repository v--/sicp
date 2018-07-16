#lang sicp

; Exercise 2.60
;
; We specified that a set would be represented as a list with no duplicates. Now suppose we allow
; duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design
; procedures element-of-set?, adjoin-set, union-set, and intersection-set that operate on this
; representation. How does the efficiency of each compare with the corresponding procedure for the
; non-duplicate representation? Are there applications for which you would use this representation
; in preference to the non-duplicate one?

; Solution
; Original definitions
(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) null]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

; The space requirements in such an implementation would obviously rise, as will the time complexity
; of element-of-set? and intersection-set (whose implementation would remain the same), but the time
; complexity of adjoin-set will become constant and the time complexity of union-set will become
; linear.

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (union-set (adjoin-set (car set2) set1) (cdr set2))))

(module+ test
  (require rackunit)

  (define a '(1 1 2 2 3 3))
  (define b '(2 3 4))
  (check-equal? (remove-duplicates (sort (intersection-set a b) <=)) '(2 3))
  (check-equal? (remove-duplicates (sort (union-set a b) <=)) '(1 2 3 4)))
