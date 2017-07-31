#lang sicp

; Exercise 1.13
;
; Prove that Fib(n) is the closest integer to phi^n / sqrt(5), where phi = (1 + sqrt(5)) / 2.

; Solution
;
; We will first prove that
;   Fib(n) = (phi^n - psi^n) / sqrt(5),                                                          (1)
; where psi = (1 - sqrt(5)) / 2.
; We begin by rewriting the Fibonacci function definition for n + 1:
;
;   Fib(n + 1) = Fib(n) + Fib(n - 1),
;   Fib(n) = Fib(n + 1) - Fib(n - 1).
;
; Now, we substitute Fib(n) with (phi^n - psi^n) in the above identity to obtain
;
;   phi^(n + 1) - psi^(n + 1) = (phi^n - psi^n) - (phi^(n - 1) - psi^(n - 1)).                   (2)
;
; We will use this identity later to rebuild the recursive definition for Fib(n).
; In order to be able to do this, we must first prove that (2) holds for all positive integers.
;
; Lemma 1:
;   (2) holds for all n >= 1.
;
; Proof:
;   We manipulate the right-hand side to obtain the left-hand side.
;
;     phi^(n + 1) - psi^(n + 1) - (phi^(n - 1) - psi^(n - 1)) =
;     = phi^(n - 1) * (phi^2 - 1) - psi^(n - 1) * (psi^2 - 1) =
;     = phi^(n - 1) * (1 + 2 * sqrt(5) + 5 - 4) / 4 - psi^(n - 1) * (1 - 2 * sqrt(5) + 5 - 4) / 4 =
;     = phi^(n - 1) * (1 + sqrt(5)) / 2 - psi^(n - 1) * (1 - sqrt(5)) / 2 =
;     = phi^n - psi^n,
;
;   thus we have proved that (2) holds for all integers n >= 1.
; QED
;
; Next, we will prove that (1) holds for all non-negative integers.
;
; Lemma 2:
;   Fib(n) = (phi^n - psi^n) / sqrt(5), n >= 0
;
; Proof:
;   We will use mathematical induction with two base cases.
;
;   Case n = 0:
;     Fib(0) = 0 and
;     (phi^0 - psi^0) / sqrt(5) = 0
;
;   Case n = 1:
;     Fib(2) = 1 and
;     (phi - psi) / sqrt(5) = ((1 + sqrt(5) / 2) - (1 - sqrt(5) / 2)) / sqrt(5) = 1
;
;   We assume that the lemma statement holds for n - 1 and n - 2 and prove it for n.
;   We add appropriate denominators to (2):
;
;     (phi^n - psi^n) / sqrt(5) = (phi^(n - 1) - psi^(n - 1)) / sqrt(5) - (phi^(n - 2) - psi^(n - 2)) / sqrt(5)
;
;   and substitute Fib(n - 1) and Fib(n - 2), since we have already assumed that this lemma holds for them:
;
;     (phi^n - psi^n) / sqrt(5) = Fib(n - 1) - Fib(n - 2).
;
;   Since Fib(n) also equals Fib(n - 1) - Fib(n - 2), by transitivity,
;     Fib(n) = (phi^n - psi^n) / sqrt(5).
;
;   This concludes the proof.
; QED
;
; In the following theorem, we will prove the main result - Fib(n) is the closest integer to phi^n / sqrt(5).
;
; Theorem:
;   | phi^n / sqrt(5) - Fib(n) | < 1 / 2.                                                        (3)
;
; Proof:
;   We utilize lemma 2 and substitute Fib(n) with (phi^n - psi^n) / sqrt(5) to obtain a simpler expression,
;
;     | psi^n / sqrt(5) | < 1 / 2,
;
;   or even
;
;     | psi^n | < sqrt(5) / 2.                                                                   (4)
;
;   We will now prove that (4) holds for n >= 0.
;   psi ~ -0.62 is less than 1, thus | psi^n | <= | psi | for any n >= 1.
;   psi^0 = 1 is larger than psi, thus | psi^n | <= 1 for any n >= 0.
;   Since sqrt(5) / 2 ~ 1.12, we can finally conclude that | psi^n | < sqrt(5) / 2 for any n >= 0.
;   We have proved (4) and, consequently, (3).
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
