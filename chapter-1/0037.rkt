#lang sicp

; Exercise 1.37
;
; a. An infinite continued fraction is an expression of the form
;
; f = ________n1________
;     d1 + ______n2_____
;          d2 + ___n3___
;               d3 + ...
;
; As an example, one can show that the infinite continued fraction expansion with the N_i
; and the D_i all equal to 1 produces 1/phi, where phi is the golden ratio (described in section 1.2.2).
; One way to approximate an infinite continued fraction is to truncate the expansion after
; a given number of terms. Such a truncation - a so-called k-term finite continued fraction -
; has the form
;
; f = _______n1______
;     d2 + ____n2____
;          d3 + . . .
;               nk/dk
;
; Suppose that n and d are procedures of one argument (the term index i) that return the N_i and D_i
; of the terms of the continued fraction. Define a procedure cont-frac such that evaluating
; (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure
; by approximating 1/phi using
;
; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)
;
; for successive values of k. How large must you make k in order to get an approximation that is
; accurate to 4 decimal places?
;
; b. If your cont-frac procedure generates a recursive process, write one that generates
; an iterative process. If it generates an iterative process, write one that generates
; a recursive process.

; Solution

(define (cont-frac-rec n d k)
  (define limit (+ k 1))
  (define (iter m)
    (if (= m limit)
        0
        (/ (n m)
           (+ (d m)
              (iter (+ m 1))))))

  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter m result)
    (if (= m 0)
        result
        (iter (- m 1)
              (/ (n m) (+ (d m) result)))))

  (iter k 0))

(define inverse-phi (/ 1.6180339887))

(provide (rename-out [cont-frac-iter cont-frac]))

(module+ test
  (require rackunit)

  ; The minimal number of iterations is 10.
  (define (check-cont-frac-impl impl)
    (check-= (impl (lambda (i) 1.0)
                   (lambda (i) 1.0)
                   10)
             inverse-phi
             1e-4))

  (check-cont-frac-impl cont-frac-rec)
  (check-cont-frac-impl cont-frac-iter))
