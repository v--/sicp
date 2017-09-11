#lang sicp

; Exercise 2.21
;
; The procedure square-list takes a list of numbers as argument and returns a list of the squares of those numbers.
;
; (square-list (list 1 2 3 4))
; (1 4 9 16)
;
; Here are two different definitions of square-list. Complete both of them by filling in the missing expressions:
;
; (define (square-list items)
;   (if (null? items)
;       nil
;       (cons <??> <??>)))
;
; (define (square-list items)
;   (map <??> <??>))

; Solution

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (square x))
       items))

(module+ test
  (require rackunit)

  (define (verify-impl impl)
    (check-equal? (impl (list 1 2 3 4))
                  (list 1 4 9 16)))

  (verify-impl square-list)
  (verify-impl square-list-map))
