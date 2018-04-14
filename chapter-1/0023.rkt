#lang sicp

(require (only-in chapter-1/0022 smallest-divisor-iterations))

; Exercise 1.23
;
; The smallest-divisor procedure shown at the start of this section does lots of needless testing:
; After it checks to see if the number is divisible by 2 there is no point in checking to see
; if it is divisible by any larger even numbers. This suggests that the values used for test-divisor
; should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, ....
; To implement this change, define a procedure next that returns 3 if its input is equal to 2
; and otherwise returns its input plus 2. Modify the smallest-divisor procedure
; to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating
; this modified version of smallest-divisor, run the test for each of the 12 primes found
; in exercise 1.22. Since this modification halves the number of test steps, you should expect it
; to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio
; of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?

; Solution
; Now we implement the optimized procedure mentioned in the exercise description and show
; that it's generally twice faster than the naive implementation. For the benchmarks we use
; a specialized procedure that outputs the iteration count rather than the actual divisor.

(define (fast-smallest-divisor n)
  (fast-find-divisor n 2))

(define (fast-find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (fast-find-divisor n (next test-divisor))]))

; The specialized iteration counters
(define (fast-smallest-divisor-iterations n)
  (define (fast-find-divisor-iterations n test-divisor iterations)
    (if (or (> (square test-divisor) n)
            (divides? test-divisor n))
        iterations
        (fast-find-divisor-iterations n (next test-divisor) (+ iterations 1))))

  (fast-find-divisor-iterations n 2 1))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(module+ test
  (require rackunit)
  (require support/fuzzy-checks)

  (define-simple-check (check-twice-faster? n tolerance)
    (fuzzy-ratio-equal? (* (fast-smallest-divisor-iterations n) 2)
                        (smallest-divisor-iterations n)
                        tolerance))

  (check-twice-faster? 1009 1e-1)
  (check-twice-faster? 1013 1e-1)
  (check-twice-faster? 1019 1e-1)

  (check-twice-faster? 10007 1e-1)
  (check-twice-faster? 10009 1e-1)
  (check-twice-faster? 10037 1e-1)

  (check-twice-faster? 100003 1e-2)
  (check-twice-faster? 100019 1e-2)
  (check-twice-faster? 100043 1e-2)

  (check-twice-faster? 1000003 1e-2)
  (check-twice-faster? 1000033 1e-2)
  (check-twice-faster? 1000037 1e-2))
