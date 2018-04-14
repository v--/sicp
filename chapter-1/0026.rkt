#lang sicp

; Exercise 1.26
;
; Louis Reasoner is having great difficulty doing exercise 1.24.
; His fast-prime? test seems to run more slowly than his prime? test.
; Louis calls his friend Eva Lu Ator over to help. When they examine Louis's code,
; they find that he has rewritten the expmod procedure to use an explicit multiplication,
; rather than calling square:

(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m)]
        [else
          (remainder (* base (expmod base (- exp 1) m))
           m)]))

; "I don't see what difference that could make," says Louis. "I do." says Eva.
; "By writing the procedure like that, you have transformed the Theta(log n) process
; into a Theta(n) process." Explain.

; Solution
; The optimization that makes fast-expmod run in logarithmic time forces fast-expmod
; to be applied two times less if the exponent is even, compared to the naive implementation.
; When written in the above way, expmod applies itself twice if the exponent is even,
; which makes the optimization pointless.
;
; In other words, the evaluation tree for expmod is just as deep as the one for fast-expmod,
; but it is much wider:
;
; Naive implementation - Theta(n):
;
; expmod(n, 5, m)
; └──expmod(n, 4, m)
;    └──expmod(n, 3, m)
;       └──expmod(n, 2, m)
;          └──expmod(n, 1, m)
;             └──expmod(n, 0, m)
;
; fast-expmod - Theta(log n):
;
; expmod(n, 5, m)
; └──expmod(n, 4, m)
;    └──expmod(n, 2, m)
;       └──expmod(n, 1, m)
;          └──expmod(n, 0, m)
;
; expmod from the exercise description Theta(2^(log_2 n)) = Theta(n):
;
; expmod(n, 5, m)
; └──expmod(n, 4, m)
;    ├──expmod(n, 2, m)
;    │  ├──expmod(n, 1, m)
;    │  │  └──expmod(n, 0, m)
;    │  │
;    │  └──expmod(n, 1, m)
;    │     └──expmod(n, 0, m)
;    │
;    └──expmod(n, 2, m)
;       ├──expmod(n, 1, m)
;       │  └──expmod(n, 0, m)
;       │
;       └──expmod(n, 1, m)
;          └──expmod(n, 0, m)
;
