#lang sicp

; Exercise 2.54
;
; Two lists are said to be equal? if they contain equal elements arranged in the same order.
; For example,
;   (equal? '(this is a list) '(this is a list))
; is true, but
;   (equal? '(this is a list) '(this (is a) list))
; is false. To be more precise, we can define equal? recursively in terms of the basic eq? equality
; of symbols by saying that a and b are equal? if they are both symbols and the symbols are eq?, or
; if they are both lists such that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b).
; Using this idea, implement equal? as a procedure.

; Solution
; This procedure doesn't handle things like numbers and strings, which are not symbols,
; but are nevertheless handled in the equal? primitive.
(define (equal? a b)
  (or (and (null? a)
           (null? b))
      (and (symbol? a)
           (symbol? b)
           (eq? a b))
      (and (list? a)
           (list? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))

(module+ test
  (require rackunit)

  ; Simple equality
  (check-true (equal? 'a 'a))

  ; Symbol list equality
  (check-true (equal? '(a b c) (list 'a 'b 'c)))

  ; Tests from the exercise description
  (check-true (equal? '(this is a list) '(this is a list)))
  (check-false (equal? '(this is a list) '(this (is a) list))))
