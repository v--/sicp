#lang sicp

(require (only-in chapter-2/0062 union-set-ordered))
(require (only-in chapter-2/0063 tree->list))
(require (only-in chapter-2/0064 list->tree))

; Exercise 2.65
;
; Use the results of exercises 2.63 and  2.64 to give (n) implementations of union-set and
; intersection-set for sets implemented as (balanced) binary trees.

; Solution

(define (intersection-set set1 set2)
  ; I decided to implement this algorithm iteratively instead of copying it from the book.
  ; The downside is that the result is in reverse order, but that doesn't really matter here.
  (define (intersection-ordered-iter list1 list2 common)
    (if (or (null? list1) (null? list2))
        common
        (let ([a (car list1)]
              [b (car list2)])
          (cond [(= a b) (intersection-ordered-iter (cdr list1) (cdr list2) (cons a common))]
                [(> a b) (intersection-ordered-iter list1 (cdr list2) common)]
                [(< a b) (intersection-ordered-iter (cdr list1) list2 common)]))))

  (list->tree (intersection-ordered-iter (tree->list set1)
                                 (tree->list set2)
                                 null)))

(define (union-set set1 set2)
  (list->tree (union-set-ordered (tree->list set1)
                                 (tree->list set2))))

(module+ test
  (require rackunit)

  (define set1 (list->tree '(1 3 5)))
  (define set2 (list->tree '(1 2 3)))

  (check-equal? (sort (tree->list (intersection-set set1 set2)) <=)
                '(1 3))

  (check-equal? (sort (tree->list (union-set set1 set2)) <=)
                '(1 2 3 5)))
