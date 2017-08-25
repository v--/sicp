#lang sicp

(require (only-in chapter-1/0043 repeated))

; Exercise 2.6

; In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language
; that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative
; integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; This representation is known as Church numerals, after its inventor, Alonzo Church, the logician
; who invented the lambda calculus.
;
; Define one and two directly (not in terms of zero and add-1). Give a direct definition
; of the addition procedure + (not in terms of repeated application of add-1).

; Solution

(module+ test
  (require rackunit)

  ; Define a helper for converting Church numerals to integers
  (define (church-numeral-cardinality numeral)
    (define calls 0)
    ((numeral (lambda (g) (set! calls (inc calls)))) void)
    calls)

  (define-simple-check (check-numeral-cardinality numeral n)
    (= (church-numeral-cardinality numeral) n))

  ; Verify that the helper works
  (check-numeral-cardinality zero 0)
  (check-numeral-cardinality (add-1 zero) 1)
  (check-numeral-cardinality ((repeated add-1 5) zero) 5)

  ; Use substitution to find and express one directly
  (define-simple-check (check-one? numeral)
    (= (church-numeral-cardinality numeral) 1))

  (check-one? (add-1 zero))
  (check-one? (lambda (f) (lambda (x) (f ((zero f) x)))))
  (check-one? (lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
  (check-one? (lambda (f) (lambda (x) (f x))))

  (define one (lambda (f) (lambda (x) (f x))))

  ; Use substitution to find and express two directly
  (define-simple-check (check-two? numeral)
    (= (church-numeral-cardinality numeral) 2))

  (check-two? (add-1 one))
  (check-two? (lambda (f) (lambda (x) (f ((one f) x)))))
  (check-two? (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x)))))
  (check-two? (lambda (f) (lambda (x) (f (f x)))))

  (define two (lambda (f) (lambda (x) (f (f x)))))

  ; Extrapolating from the definitions for one and two, we build a procedure
  ; that yields closed forms for the n-th Church numeral
  (define (church-factory n)
    (lambda (f) (lambda (x) ((repeated f n) x))))

  (check-numeral-cardinality (church-factory 4) 4)
  (check-numeral-cardinality (church-factory 13) 13)

  ; Finally, we define a generic addition procedure
  (define (add a b)
    (lambda (f)
      (lambda (x)
        ((a f) ((b f) x)))))

  (check-numeral-cardinality (add one zero) 1)
  (check-numeral-cardinality (add (church-factory 4) (church-factory 9)) 13))
