#lang sicp

(require (only-in chapter-2/0038 fold-left fold-right))

; Exercise 2.39
;
; Complete the following definitions of reverse (exercise 2.18) in terms of fold-right and fold-left
; from exercise 2.38:
;
; (define (reverse-foldr sequence)
;   (fold-right (lambda (x y) <??>) null sequence))
;
; (define (reverse-foldl sequence)
;   (fold-left (lambda (x y) <??>) null sequence))

; Solution

(define (reverse-foldr sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse-foldl sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(module+ test
  (require rackunit)

  (check-equal? (reverse-foldr (list 1 2 3))
                (list 3 2 1))

  (check-equal? (reverse-foldr (list 1 2 3))
                (reverse-foldl (list 1 2 3))))
