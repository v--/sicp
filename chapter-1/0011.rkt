#lang sicp

; Exercise 1.11
;
; A function f is defined by the rule that
; f(n) = n if n < 3
; f(n) = f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3) if n >= 3.

; Write a procedure that computes f by means of a recursive process.
; Write a procedure that computes f by means of an iterative process.

; Solution

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (f-impl a b c count)
    (cond [(= count 0) c]
          [(= count 1) b]
          [(= count 2) a]
          [else (f-impl (+ a (* 2 b) (* 3 c)) a b (- count 1))]))
  (f-impl 2 1 0 n))

; Verify that the iterative procedure yields correct values
(check-equal? (f-iter 0) 0)
(check-equal? (f-iter 1) 1)
(check-equal? (f-iter 2) 2)
(check-equal? (f-iter 3) 4)
(check-equal? (f-iter 5) 25)
(check-equal? (f-iter 10) 1892)

; Verify that the two implementation match
(check-equal? (f-rec 0) (f-iter 0))
(check-equal? (f-rec 1) (f-iter 1))
(check-equal? (f-rec 2) (f-iter 2))
(check-equal? (f-rec 3) (f-iter 3))
(check-equal? (f-rec 5) (f-iter 5))
(check-equal? (f-rec 10) (f-iter 10))
