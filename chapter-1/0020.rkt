#lang sicp

; Exercise 1.20
; The process that a procedure generates is of course dependent on the rules used by the interpreter.
; As an example, consider the iterative GCD procedure given above. Suppose we were to interpret
; this procedure using normal-order evaluation, as discussed in section 1.1.5.
; (The normal-order-evaluation rule for if is described in exercise 1.5.)
; Using the substitution method (for normal order), illustrate the process generated in evaluating
; (gcd 206 40) and indicate the remainder operations that are actually performed. How many
; remainder operations are actually performed in the normal-order evaluation of (gcd 206 40)?
; In the applicative-order evaluation?

; Solution
; Original definitions

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal-order substitution shows that the remainder procedure is applied 11 times.
; This can be easily seen by aliasing repeated applications of remainder and calculating
; how much times each one of the aliases is evaluated.

(define-simple-check (check-two? x) (= x 2))

(check-two? (gcd 206 40))
(check-two? (gcd 40 (remainder 206 40)))

(define r1 (remainder 206 40))

(check-two? (if (= r1 0)
                40
                (gcd r1 (remainder 40 r1))))

(define r2 (remainder 40 r1))

(check-two? (if (= r2 0)
                r1
                (gcd r2 (remainder r1 r2))))

(define r3 (remainder r1 r2))

(check-two? (if (= r3 0)
                r2
                (gcd r3 (remainder r2 r3))))

(define r4 (remainder r2 r3))

(check-two? (if (= r4 0)
                r3
                (gcd r4 (remainder r3 r4))))

(check-two? r3)

; In the end, the if form evaluates r4 as part of the condition and r3 as the "then" branch.
;
; The number of calls to remainder for each of the aliases is:
;   r1 - 1 direct call
;   r2 - 2 calls = 1 direct call and 1 evaluation of r1
;   r3 - 4 calls = 1 direct call, 1 evaluation of r1 and 1 evaluation of r2
;   r4 - 7 calls = 1 direct call, 1 evaluation of r2 and 1 evaluation of r3
;
; Thus remainder is evaluated a total of 4 + 7 = 11 times.

; The applicative-order substitution, on the other hand, applies remainder only 4 times:
(check-two? (gcd 206 40))
(check-two? (gcd 40 (remainder 206 40)))
(check-two? (gcd 40 6))
(check-two? (gcd 6 (remainder 40 6)))
(check-two? (gcd 6 4))
(check-two? (gcd 4 (remainder 6 4)))
(check-two? (gcd 4 2))
(check-two? (gcd 2 (remainder 4 2)))
(check-two? (gcd 0 2))
(check-two? 2)
