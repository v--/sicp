#lang sicp

; Exercise 2.53
;
; What would the interpreter print in response to evaluating each of the following expressions?

; Solution

(module+ test
  (require rackunit)

  (check-equal? (list 'a 'b 'c)
                '(a b c))

  (check-equal? (list (list 'george))
                '((george)))

  (check-equal? (cdr '((x1 x2) (y1 y2)))
                '((y1 y2)))

  (check-equal? (cadr '((x1 x2) (y1 y2)))
                '(y1 y2))

  (check-equal? (pair? (car '(a short list)))
                false)

  (check-equal? (memq 'red '((red shoes) (blue socks)))
                false)

  (check-equal? (memq 'red '(red shoes blue socks))
                '(red shoes blue socks)))
