#lang sicp

; Exercise 1.25
;
; Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod.
; After all, she says, since we already know how to compute exponentials,
; we could have simply written

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; Is she correct? Would this procedure serve as well for our fast prime tester? Explain.

; Solution
; Original definitions

(define (square x)
  (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (fast-expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (fast-expmod base (- exp 1) m))
                     m))))

; Since fast-expmod takes the remainder at every step, it works with smaller numbers.
; Depending on how the interpreter handles numbers, running the expmod procedure
; from the exercise description on very large numbers will either cause it to overflow
; or to slow down (if the interpreter supports arbitrary-precision integers).
