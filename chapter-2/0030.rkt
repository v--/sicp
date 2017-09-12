#lang sicp

; Exercise 2.30
;
; Define a procedure square-tree analogous to the square-list procedure of exercise 2.21. That is,
; square-list should behave as follows:
;
; (square-tree
;   (list 1
;         (list 2 (list 3 4) 5)
;         (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
;
; Define square-tree both directly (i.e., without using any higher-order procedures) and also
; by using map and recursion.

; Solution

(define (square-tree tree)
  (cond [(null? tree) null]
        [(number? tree) (square tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (number? sub-tree)
             (square sub-tree)
             (square-tree-map sub-tree)))
       tree))

(module+ test
  (require rackunit)

  (define src (list 1
                    (list 2 (list 3 4) 5)
                    (list 6 7)))

  (define dest (list 1
                     (list 4 (list 9 16) 25)
                     (list 36 49)))

  (check-equal? (square-tree src) dest)
  (check-equal? (square-tree-map src) dest))
