#lang sicp

; Exercise 2.56
;
; Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement
; the differentiation rule
;    d(u^n) / dr = n * u^(n - 1) * (du / dx).
; By adding a new clause to the deriv program and defining appropriate procedures exponentiation?, base, exponent,
; and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in the rules that anything
; raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.

; Solution
; Original definitions:
; 1. Variables
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; 2. Sums
(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list '+ a1 a2)]))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

; 3. Products
(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

; 4. Misc
(define (=number? exp num)
  (and (number? exp) (= exp num)))

; Required constructor and selectors
(define (make-exponentiation base exponent)
  (cond [(variable? exponent) (error "Variable exponents are not supported")]
        [(and (=number? base 0) (=number? exponent 0)) (error "0 ** 0 is undefined")]
        [(=number? base 0) 0]
        [(=number? base 1) 1]
        [(=number? exponent 1) base]
        [(number? base) (expt base exponent)]
        [else (list '** base exponent)]))

(define (exponentiation? expression)
  (and (pair? expression) (eq? (car expression) '**)))

(define (base expression)
  (cadr expression))

(define (exponent expression)
  (caddr expression))

; Extended deriv implementation
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp)))]
        [(exponentiation? exp)
         (make-product (deriv (base exp) var)
                       (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (dec (exponent exp)))))
         ]
        [else
         (error "unknown expression type -- DERIV" exp)]))

(provide variable? same-variable?
         make-sum sum? addend augend
         make-product product? multiplier multiplicand
         =number?
         make-exponentiation exponentiation? base exponent
         deriv)

(module+ test
  (require rackunit)

  ; Main construction cases
  (check-equal? (make-exponentiation 2 3)
                8)

  (check-equal? (make-exponentiation 'x 1)
                'x)

  (check-equal? (make-exponentiation '(* x y) 10)
                '(** (* x y) 10))

  ; Degenerate construction cases
  (check-equal? (make-exponentiation 0 2)
                0)

  (check-equal? (make-exponentiation 2 0)
                1)

  (check-equal? (make-exponentiation 1 2)
                1)

  (check-equal? (make-exponentiation 2 1)
                2)

  ; Main differentiation cases
  (check-equal? (deriv '(** x 20) 'x)
                '(* 20 (** x 19)))

  ; This one can be simplified even further if we tweak the constructors to handle recursive cases
  (check-equal? (deriv '(** (** x 2) 20) 'x)
                '(* (* 2 x) (* 20 (** (** x 2 ) 19))))

  ; Degenerate differentiation cases
  (check-equal? (deriv 'x 'x)
                1)

  (check-equal? (deriv 10 'x)
                0)

  (check-equal? (deriv '(** 0 10) 'x)
                0)

  (check-equal? (deriv '(** x 2) 'x)
                '(* 2 x)))
