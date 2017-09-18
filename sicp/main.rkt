#lang racket/base

(require racket)
(provide (all-from-out racket))

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

(define (average . args)
  (/ (apply + args) (length args)))

(provide (all-defined-out))
