#lang sicp

; Exercise 1.38
;
; In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis,
; which included a continued fraction expansion for e - 2, where e is the base of the natural
; logarithms. In this fraction, the N_i are all 1, and the D_i are successively
;   1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....
;
; Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e,
; based on Euler's expansion.

; Solution
; Original definitions

(define (cont-frac n d k)
  (define (iter m result)
    (if (= m 0)
        result
        (iter (- m 1)
              (/ (n m) (+ (d m) result)))))

  (iter k 0))

; Verify that the lambda for D_i works
(#%require rackunit)

(check-= (cont-frac (lambda (i) 1.0)
                    (lambda (i) (if (= (remainder i 3) 2)
                                    (* (+ (quotient i 3) 1) 2)
                                    1))
                    100)
         (- (exp 1) 2)
         1e-4)
