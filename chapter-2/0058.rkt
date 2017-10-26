#lang sicp

(require (only-in chapter-2/0056 variable? same-variable? augend multiplicand =number?))
(require (only-in chapter-2/0057 deriv-factory))

; Exercise 2.58
;
; Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation,
; in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms
; of abstract data, we can modify it to work with different representations of expressions solely by changing
; the predicates, selectors, and constructors that define the representation of the algebraic expressions on which
; the differentiator is to operate.
;
; a. Show how to do this in order to differentiate algebraic expressions presented in infix form,
; such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments
; and that expressions are fully parenthesized.
;
; b. The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 * (x + y + 2)),
; which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design
; appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?

; Solution
; a. These are the procedures from the book, slightly modified to use infix notation instead of
; prefix notation.
(define (make-sum-a a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list a1 '+ a2)]))

(define (sum?-a x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend-a s) (car s))

(define (make-product-a m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list m1 '* m2)]))

(define (product?-a x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier-a p) (car p))

(define deriv-a (deriv-factory make-sum-a sum?-a addend-a augend
                               make-product-a product?-a multiplier-a multiplicand))

; b. Reuse make-sum-a and make-product-a and provide custom predicates and selectors, preserving
; the original deriv procedure.
(define (helper exp)
  (if (null? (cdr exp))
      (car exp)
      exp))

(define (make-main-selector symbol)
  (define (main-selector exp)
    (define (iter exp)
      (if (eq? (car exp) symbol)
          null
          (cons (car exp) (iter (cdr exp)))))
    (helper (iter exp)))

  main-selector)

(define (make-rest-selector symbol)
  (define (rest-selector exp)
    (helper (cdr (memq symbol exp))))

  rest-selector)

(define (sum?-b exp)
  (and (pair? exp) (memq '+ exp)))

(define addend-b (make-main-selector '+))
(define augend-b (make-rest-selector '+))

(define (product?-b exp)
  (and (not (sum?-b exp)) (pair? exp) (memq '* exp)))

(define multiplier-b (make-main-selector '*))
(define multiplicand-b (make-rest-selector '*))

(define deriv-b (deriv-factory make-sum-a sum?-b addend-b augend-b
                               make-product-a product?-b multiplier-b multiplicand-b))

(module+ test
  (require rackunit)

  ; a.
  (check-equal? (deriv-a '(x + (3 * (x + (y + 2)))) 'x)
                4)

  ; b.
  (check-equal? (deriv-b '(x + (3 * (x + (y + 2)))) 'x) ; Example from a.
                (deriv-b '(x + 3 * (x + y + 2)) 'x)) ; Example from b.

  ; Double differentiation
  (check-equal? (deriv-b (deriv-b '(x + 3 * (x + y + 2)) 'x) 'x)
                0))
