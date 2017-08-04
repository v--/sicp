#lang sicp

; Exercise 1.25
;
; Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod.
; After all, she says, since we already know how to compute exponentials,
; we could have simply written

(define (naive-expmod base exp m)
  (remainder (expt base exp) m))

; Is she correct? Would this procedure serve as well for our fast prime tester? Explain.

; Solution
; Since expmod from exercise 1.24 takes the remainder at every step, it works with smaller numbers.
; Depending on how the interpreter handles numbers, running the naive-expmod procedure
; from the exercise description on very large numbers will either cause it to overflow
; or to slow down (if the interpreter supports arbitrary-precision integers).
