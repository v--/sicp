#lang sicp

; Exercise 2.31
;
; Abstract your answer to exercise 2.30 to produce a procedure tree-map with the property that
; square-tree could be defined as

(define (square-tree tree) (tree-map square tree))

; Solution

(define (tree-map proc tree)
  (cond [(null? tree) null]
        [(number? tree) (proc tree)]
        [else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree)))]))

(module+ test
  (require rackunit)

  (define src (list 1
                    (list 2 (list 3 4) 5)
                    (list 6 7)))

  (define dest (list 1
                     (list 4 (list 9 16) 25)
                     (list 36 49)))

  (check-equal? (square-tree src) dest))
