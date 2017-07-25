#lang sicp

; Exercise 1.13
;
; Prove that Fib(n) is the closest integer to phi^n / sqrt(5), where phi = (1 + sqrt(5)) / 2.

; Solution
;
; First, will prove that Fib(n) = (phi^n - psi^n) / sqrt(5), where psi = (1 - sqrt(5)) / 2.
;
; This will be proven using two additional lemmas:
;
; Lemma 1:
;   phi^(n + 1) - psi^(n + 1) = (phi^n - psi^n) - (phi^(n - 1) - psi^(n - 1)).
;
; Proof:
;   We first rewrite the Fibonacci function definition
;
;     Fib(n + 1) = Fib(n) + Fib(n - 1)
;     Fib(n) = Fib(n + 1) - Fib(n - 1)
;
;   Now we substitute Fib(n) with (phi^n - psi^n) / sqrt(5) in the right-hand side of the above identity.
;   (the denominator - sqrt(5) - is omitted for brevity)
;
;     phi^(n + 1) - psi^(n + 1) - (phi^(n - 1) - psi^(n - 1)) =
;     = phi^(n - 1) * (phi^2 - 1) - psi^(n - 1) * (psi^2 - 1) =
;     = phi^(n - 1) * (1 + 2 * sqrt(5) + 5 - 4) / 4 - psi^(n - 1) * (1 - 2 * sqrt(5) + 5 - 4) / 4 =
;     = phi^(n - 1) * (1 + sqrt(5)) / 2 - psi^(n - 1) * (1 - sqrt(5)) / 2 =
;     = phi^n - psi^n,
;
;   thus, we proved that
;
;     phi^(n + 1) - psi^(n + 1) = (phi^n - psi^n) - (phi^(n - 1) - psi^(n - 1)).
; QED
;
; Lemma 2:
;   Fib(n) = (phi^n - psi^n) / sqrt(5)
;
; Proof:
;   We will use mathematical induction with two base cases
;
;   Case n = 0:
;     Fib(0) = 0 and
;     (phi^0 - psi^0) / sqrt(5) = 0
;
;   Case n = 1:
;     Fib(1) = 1 and
;     (phi - psi) / sqrt(5) = ((1 + sqrt(5) / 2) - (1 - sqrt(5) / 2)) / sqrt(5) = 1
;
;   We assume that the lemma statement holds for n and n - 1 and prove it for n + 1.
;   We use the results from lemma 1:
;
;     phi^(n + 1) - psi^(n + 1) = (phi^n - psi^n) - (phi^(n - 1) - psi^(n - 1))
;
;   and substitute Fib(n) and Fib(n - 1), since we assume that this lemma holds for them:
;
;     phi^(n + 1) - psi^(n + 1) = sqrt(5) * (Fib(n) - Fib(n - 1)).
;
;   Since Fib(n + 1) also equates Fib(n) - Fib(n - 1), by transitivity,
;     Fib(n + 1) = (phi^(n + 1) - psi^(n + 1)) / sqrt(5).
; QED
;
; Now, we prove the main result.
;
; Theorem:
;   Fib(n) is the closest integer to phi^n / sqrt(5)
;
; Proof:
;   The theorem statement can be formulated more precisely with the inequality
;
;     | phi^n / sqrt(5) - Fib(n) | < 1 / 2.
;
;   If we substitute the result from lemma 2, we obtain a the simpler expression,
;
;     | psi^n / sqrt(5) | < 1 / 2,
;
;   or even
;
;     | psi^n | < sqrt(5) / 2,
;
;   Since psi ~= -0.62 < 1, we can conclude that | psi^n | <= | psi | for any n >= 1.
;   Since psi^0 = 1 and psi < 1, we can also conclude that | psi^n | <= 1 for any n >= 0.
;   Since sqrt(5) / 2 ~= 1.12, we can finally conclude that | psi^n | < sqrt(5) / 2.
; QED

; Here are are few tests to verify that Fib(n) = (phi^n - psi^n) / sqrt(5)

(define (fib1 n)
  (if (< n 2)
      n
      (+ (fib1 (- n 1)) (fib1 (- n 2)))))

(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (/ (- 1 (sqrt 5)) 2))

(define (fib2 n)
  (/ (- (expt phi n) (expt psi n)) (sqrt 5)))

(#%require rackunit)

(check-= (fib1 1) (fib2 1) 1e-3)
(check-= (fib1 5) (fib2 5) 1e-3)
(check-= (fib1 10) (fib2 10) 1e-3)
