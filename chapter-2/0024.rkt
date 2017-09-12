#lang sicp

; Exercise 2.24
;
; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed
; by the interpreter, the corresponding box-and-pointer structure, and the interpretation
; of this as a tree (as in figure 2.6).

; Solution
; Original definitions

(define (count-leaves x)
  (cond [(null? x) 0]
        [(not (pair? x)) 1]
        [else (+ (count-leaves (car x))
                 (count-leaves (cdr x)))]))

; The box-and-pointer structure looks like
;
; [1 *] ─→ [* /]
;           ↓
;          [2 *] ─→ [* /]
;                    ↓
;                   [3 *] ─→ [4 /]

; The tree interpretation looks like
;
; (1 (2 (3 4)))
; ├──1
; └──(2 (3 4))
;    ├──2
;    └──(3 4)
;       ├──3
;       └──4

; Both show that there are exactly 4 leaves

(module+ test
  (require rackunit)

  (define tree (list 1 (list 2 (list 3 4))))

  (check-equal? (count-leaves tree) 4))
