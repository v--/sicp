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

(module+ test
  (require rackunit)
  (require support/fuzzy-checks)

  ; We verify that the fast-prime algorithm's iterations grow ~2 times when testing the smallest
  ; primes that are larger than, respectively, 1e4 and 1e8. Again, we use a specialized procedure
  ; to count the iterations of the expmod procedure, which happens to be the procedure that actually
  ; affects performance.

  (define (fast-prime?-iterations n times [iterations 0])
    (define (expmod-iterations base exp m)
      (set! iterations (+ iterations 1))
      (cond [(= exp 0) 1]
            [(even? exp)
             (remainder (square (expmod-iterations base (/ exp 2) m))
                        m)]
            [else
             (remainder (* base (expmod-iterations base (- exp 1) m))
                        m)]))

    (define (fermat-test-iterations n)
      (define (try-it a)
        (= (expmod-iterations a n n) a))
      (try-it (+ 1 (random (- n 1)))))

    (cond [(= times 0) iterations]
          [(fermat-test-iterations n) (fast-prime?-iterations n (- times 1) iterations)]
          [else iterations]))

  (let ([t1 (fast-prime?-iterations 10037 10)]
        [t2 (fast-prime?-iterations 100000039 10)])

    (check/= (* t1 2) t2 0.03)))
