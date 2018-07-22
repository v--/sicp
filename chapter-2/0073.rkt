#lang sicp

(require support/operation-table)
(require (only-in chapter-2/0056 variable? same-variable? make-sum make-product make-exponentiation))

; Exercise 2.73
;
; Section 2.3.2 described a program that performs symbolic differentiation:
;
; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum
;            (make-product (multiplier exp)
;                          (deriv (multiplicand exp) var))
;            (make-product (deriv (multiplier exp) var)
;                          (multiplicand exp))))
;         <more rules can be added here>
;         (else (error "unknown expression type -- DERIV" exp))))
;
; We can regard this program as performing a dispatch on the type of the
; expression to be differentiated. In this situation the "type tag" of the datum
; is the algebraic operator symbol (such as +) and the operation being performed
; is deriv. We can transform this program into data-directed style by rewriting
; the basic derivative procedure as

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [else ((get 'deriv (operator exp)) (operands exp) var)]))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a. Explain what was done above. Why can't we assimilate the predicates number? and same-variable?
; into the data-directed dispatch?
;
; b. Write the procedures for derivatives of sums and products, and the auxiliary code required to
; install them in the table used by the program above.
;
; c. Choose any additional differentiation rule that you like, such as the one for exponents
; (exercise 2.56), and install it in this data-directed system.
;
; d. In this simple algebraic manipulator the type of an expression is the algebraic operator that
; binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the
; dispatch line in deriv looked like
;   ((get (operator exp) 'deriv) (operands exp) var)
; What corresponding changes to the derivative system are required?

; Solution
; a. deriv was rewritten to take advantage of data-directed dispatch by indexing the table using
; the expression's operator. Naturally, since number and variable expressions do not have operators,
; we cannot index the table without introducing special type tags for them.

; b. These two deriv implementations are simply copied from the book.

(define (install-deriv-sum-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (car exp) var)
              (deriv (cadr exp) var)))

  (put 'deriv '+ deriv-sum)
  'done)

(define (install-deriv-product-package)
  (define (deriv-product exp var)
    (make-sum
      (make-product (car exp)
                    (deriv (cadr exp) var))
      (make-product (deriv (car exp) var)
                    (cadr exp))))

  (put 'deriv '* deriv-product)
  'done)

; c. This implementation is copied from 2.56 and thus cases like x^x are not handled.

(define (install-deriv-exponentiation-package)
  (define (deriv-exp exp var)
    (make-product (deriv (car exp) var)
                  (make-product (cadr exp)
                                (make-exponentiation (car exp)
                                                     (dec (cadr exp))))))

  (put 'deriv '** deriv-exp)
  'done)

; d. We only need to change the order of the symbols in the put procedure, i.e. replace
;   (put 'deriv '+ deriv-sum)
; with
;   (put '+ 'deriv deriv-sum)

(define (deriv-2 exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [else ((get (operator exp) 'deriv-2) (operands exp) var)]))

(define (install-deriv-2-sum-package)
  (define (deriv-2-sum exp var)
    (make-sum (deriv-2 (car exp) var)
              (deriv-2 (cadr exp) var)))

  (put '+ 'deriv-2 deriv-2-sum)
  'done)

(module+ test
  (require rackunit)

  (void (install-deriv-sum-package))
  (void (install-deriv-product-package))
  (void (install-deriv-exponentiation-package))

  ; Verify the basic functionality
  (check-equal? (deriv 10 'x)
                0)

  (check-equal? (deriv 'x 'x)
                1)

  ; Verify b.
  (check-equal? (deriv '(+ x x) 'x)
                2)

  (check-equal? (deriv '(* 3 x) 'x)
                '3)

  ; Verify c.
  (check-equal? (deriv '(** x 2) 'x)
                '(* 2 x))

  ; Verify d.
  (void (install-deriv-2-sum-package))

  (check-equal? (deriv '(+ x x) 'x)
                (deriv-2 '(+ x x) 'x)))
