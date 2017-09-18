#lang sicp

; Exercise 2.27
;
; Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that
; takes a list as argument and returns as its value the list with its elements reversed
; and with all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))

; x
; ((1 2) (3 4))
;
; (reverse x)
; ((3 4) (1 2))

(module+ test
  (require rackunit)

  (check-equal? (deep-reverse x)
                (list (list 4 3) (list 2 1))))

; Solution

(define (deep-reverse items)
  (if (pair? items)
      (reverse (map deep-reverse items))
      items))