#lang sicp

; Exercise 1.10
; The following procedure computes a mathematical function called Ackermann's function.

(define (A x y)
  (cond [(= y 0) 0]
        [(= x 0) (* 2 y)]
        [(= y 1) 2]
        [else (A (- x 1)
                 (A x (- y 1)))]))

; What are the values of the following expressions?
; (A 1 10)
; (A 2 4)
; (A 3 3)

; Consider the following procedures, where A is the procedure defined above:

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

; Give concise mathematical definitions for the functions computed
; by the procedures f, g, and h for positive integer values of n.
; For example, (k n) computes 5n2.

; Solution

(module* test #f
  (require rackunit)

  ; This exercise is more easily done backwards. First, we find closed-form expressions for `f`, `g` and `h`.

  ; f(n) = A(0, n) = 2 * n
  (check-equal? (f 3) 6)

  ; g(n) = 2^n, because:
  ; g(n) = {
  ;   A(1, n) = A(0, A(1, n - 1)) = f(g(n - 1)) = 2 * g(n - 1), if n > 1
  ;   2                                                       , if n = 1
  ; }
  (check-equal? (g 3) 8)

  ; h(n) = 2^...(2^(2^2)) - n twos and n - 1 exponentiations
  ; h(n) = {
  ;   n > 1, h(n) = A(2, n) = A(1, A(2, n - 1)) = g(h(n - 1)) = 2^(h(n - 1)), if n > 1
  ;   2                                                                     , if n = 1
  ; }
  (check-equal? (h 3) 16)

  ; Then, we formulate the expressions from the first part of the exercise using the above functions
  (check-equal? (A 1 10) (g 10))
  (check-equal? (A 1 10) 1024)

  (check-equal? (A 2 4) (h 4))
  (check-equal? (A 2 4) 65536)

  ; The third expression is a little trickier
  ; A(3, 3) = A(2, A(3, 2)) = A(2, A(2, A(3, 1))) = A(2, A(2, 2)) = h(h(2))
  (check-equal? (A 3 3) (h (h 2)))
  (check-equal? (A 3 3) (h 4))
  (check-equal? (A 3 3) 65536)

  ; The second and third expressions even yield the same result
  (check-equal? (A 2 4) (A 3 3)))
