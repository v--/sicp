#lang sicp

; Exercise 1.30
;
; The sum procedure above generates a linear recursion. The procedure can be rewritten so that
; the sum is performed iteratively. Show how to do this by filling in the missing expressions
; in the following definition:
;
; (define (sum term a next b)
;   (define (iter a result)
;     (if <??>
;         <??>
;         (iter <??> <??>)))
;   (iter <??> <??>))

; Solution

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(module+ test
  (require rackunit)

  (define (identity x) x)
  (define (double-inc x) (+ x 2))
  (check-equal? (sum identity 0 inc 10) 55)
  (check-equal? (sum identity 0 double-inc 10) 30)

  (define (cube x) (* x x x))
  (check-equal? (sum cube 0 inc 10) 3025))
