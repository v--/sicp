#lang racket/base

(require racket)
(provide (all-from-out racket))

(require rackunit)
(provide (all-from-out rackunit))

(define default-tolerance 1e-3)

(define runtime current-process-milliseconds)

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (divides? a b)
  (= (remainder b a) 0))

(provide (all-defined-out))
