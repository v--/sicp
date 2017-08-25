#lang sicp

; Exercise 1.24
;
; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the Fermat method),
; and test each of the 12 primes you found in that exercise. Since the Fermat test has
; Theta(log n) growth, how would you expect the time to test primes near 1 000 000 to compare
; with the time needed to test primes near 1000? Do your data bear this out?
; Can you explain any discrepancy you find?

; Solution
; Original definitions

(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
          (remainder (* base (expmod base (- exp 1) m))
                     m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

; We verify that the fast-prime algorithm's running time grows ~2 times when testing the smallest
; primes that are larger than, respectively, 1000 and 1 000 000. A large tolerance is chosen
; because of both the non-determinism the algorithm and computer performance fluctuations.

(module+ test
  (require rackunit)
  (require support/measure-procedure)
  (require support/fuzzy-checks)

  (let ([t1 (measure-procedure fast-prime? 1009 20)]
        [t2 (measure-procedure fast-prime? 1000003 20)]
        [complexity-ratio (/ (log 1e6) (log 1e3))])

    (check/= (* t1 complexity-ratio) t2 0.5)))
