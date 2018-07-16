#lang sicp

(require (only-in chapter-2/0063 entry left-branch right-branch make-tree))

; Exercise 2.64
;
; The following procedure list->tree converts an ordered list to a balanced binary tree. The helper
; procedure partial-tree takes as arguments an integer n and list of at least n elements and
; constructs a balanced tree containing the first n elements of the list. The result returned by
; partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the
; list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons null elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result (partial-tree elts left-size)])
          (let ([left-tree (car left-result)]
                [non-left-elts (cdr left-result)]
                [right-size (- n (+ left-size 1))])
            (let ([this-entry (car non-left-elts)]
                  [right-result (partial-tree (cdr non-left-elts)
                                              right-size)])
              (let ([right-tree (car right-result)]
                    [remaining-elts (cdr right-result)])
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a. Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree
; produced by list->tree for the list (1 3 5 7 9 11).
;
; b. What is the order of growth in the number of steps required by list->tree to convert a list of
; n elements?

; Solution
; a. The partial-tree procedure works by calculating a pivot index (that is roughly in the center of
; 1,...,n) and then recursively building trees for all elements to the left of the pivot and to the
; right of the pivot.
;
; The full evaluation tree for partial-tree (shortened to P) is
;   P([1 3 5 7 9 11], 6) = {5 {1 null 3} {9 7 11}}
;   ├──P([1 3 5 7 9 11], 2) = {1 null 3}
;   │  ├──P([1 3 5 7 9 11], 0) = null
;   │  └──P([3 5 7 9 11], 1) = {3 null null}
;   │     ├──P([3 5 7 9 11], 0) = null
;   │     └──P([5 7 9 11], 0) = null
;   │
;   └──P([7 9 11], 2) = {9 7 11}
;      ├──P([7 9 11], 1) = {7 null null}
;      │  ├──P([7 9 11], 0) = null
;      │  └──P([9 11], 0) = null
;      │
;      └──P([11], 1) = {11 null null}
;         ├──P([11], 0) = null
;         └──P([], 0) = null
;
; b. The order of growth in the number of steps is linear.

(provide list->tree)

(module+ test
  (require rackunit)

  (check-equal? (list->tree '(1 3 5 7 9 11))
                (make-tree 5
                           (make-tree 1
                                      null
                                      (make-tree 3 null null))
                           (make-tree 9
                                      (make-tree 7 null null)
                                      (make-tree 11 null null)))))
