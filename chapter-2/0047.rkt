#lang sicp

; Exercise 2.47
;
; Here are two possible constructors for frames:

(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-pairs origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; For each constructor supply the appropriate selectors to produce an implementation for frames.

; Solution
; Selectors for the first constructor
(define (origin-frame-list frame)
  (car frame))

(define (edge1-frame-list frame)
  (cadr frame))

(define (edge2-frame-list frame)
  (caddr frame))

; Selectors for the second constructor
(define (origin-frame-pairs frame)
  (car frame))

(define (edge1-frame-pairs frame)
  (cadr frame))

(define (edge2-frame-pairs frame)
  (cddr frame))

(provide (rename-out [make-frame-list make-frame]
                     [origin-frame-list origin-frame]
                     [edge1-frame-list edge1-frame]
                     [edge2-frame-list edge2-frame]))

; Solution

(module+ test
  (require rackunit)
  (require (only-in chapter-2/0046 make-vect))

  (define origin (make-vect 0 0))
  (define edge1 (make-vect 0 1))
  (define edge2 (make-vect 1 0))

  (define frame-list (make-frame-list origin edge1 edge2))
  (check-equal? (origin-frame-list frame-list) origin)
  (check-equal? (edge1-frame-list frame-list) edge1)
  (check-equal? (edge2-frame-list frame-list) edge2)

  (define frame-pairs (make-frame-pairs origin edge1 edge2))
  (check-equal? (origin-frame-pairs frame-pairs) origin)
  (check-equal? (edge1-frame-pairs frame-pairs) edge1)
  (check-equal? (edge2-frame-pairs frame-pairs) edge2))
