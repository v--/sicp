#lang sicp

; Exercise 1.33
;
; You can obtain an even more general version of accumulate (exercise 1.32) by introducing
; the notion of a filter on the terms to be combined. That is, combine only those terms derived
; from values in the range that satisfy a specified condition. The resulting filtered-accumulate
; abstraction takes the same arguments as accumulate, together with an additional predicate of
; one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to
; express the following using filtered-accumulate:
;
; a. the sum of the squares of the prime numbers in the interval a to b
; (assuming that you have a prime? predicate already written)
;
; b. the product of all the positive integers less than n that are relatively prime to n
; (i.e., all positive integers i < n such that GCD(i, n) = 1).

; Solution

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond [(> a b) result]
          [(filter a) (iter (next a) (combiner result (term a)))]
          [else (iter (next a) result)]))
  (iter a null-value))

; Define and verify sum-of-prime-squares

(#%require math) ; for prime? and coprime?

(define (square x)
  (* x x))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(#%require rackunit)

(check-equal? (sum-of-prime-squares 1 10) 87)

; Define and verify product-of-coprimes

(define (product-of-coprimes n)
  (define (coprime-to-n? m)
    (coprime? n m))

  (filtered-accumulate * 1 identity 1 inc (dec n) coprime-to-n?))

(check-equal? (product-of-coprimes 10) 189)
