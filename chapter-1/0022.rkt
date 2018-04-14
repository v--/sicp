#lang sicp

(require (only-in chapter-1/0021 smallest-divisor))

; Exercise 1.22
;
; Most Lisp implementations include a primitive called runtime that returns an integer
; that specifies the amount of time the system has been running (measured, for example,
; in microseconds). The following timed-prime-test procedure, when called with an integer n,
; prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks
; followed by the amount of time used in performing the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (when (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; Using this procedure, write a procedure search-for-primes that checks the primality
; of consecutive odd integers in a specified range. Use your procedure to find
; the three smallest primes larger than 1000; larger than 10 000; larger than 100 000;
; larger than 1 000 000. Note the time needed to test each prime. Since the testing algorithm
; has order of growth of Theta(sqrt(n)), you should expect that testing for primes around 10 000
; should take about sqrt(10) times as long as testing for primes around 1000.
; Do your timing data bear this out? How well do the data for 100 000 and 1 000 000
; support the n prediction? Is your result compatible with the notion that programs on your machine
; run in time proportional to the number of steps required for the computation?

; Solution
; Original definitions

(define (prime? n)
  (= n (smallest-divisor n)))

; We will divert from the precise exercise requirements to allow automated tests.
; First, we write a search-for-primes procedure that returns a list
; with the three smallest prime numbers that are larger than n.

(define (search-for-primes n)
  (define (search-for-primes-iter i result)
    (if (= (length result) 3)
        result
        (search-for-primes-iter (+ i 2)
                                (if (prime? i)
                                    (cons i result)
                                    result))))

  (search-for-primes-iter (if (odd? n) n (+ n 1)) null))

; This procedure counts the find-divisor iterations by outputting (D - 1) where D is the largest divisor that is tested.
(define (smallest-divisor-iterations n)
  (define (find-divisor-iterations n test-divisor)
    (if (or (> (square test-divisor) n)
            (divides? test-divisor n))
        test-divisor
        (find-divisor-iterations n (+ test-divisor 1))))

    (- (find-divisor-iterations n 2) 1))

(provide smallest-divisor-iterations)

(module+ test
  (require rackunit)
  (require support/fuzzy-checks)

  (check-equal? (search-for-primes 1000) (list 1019 1013 1009))
  (check-equal? (search-for-primes 10000) (list 10037 10009 10007))
  (check-equal? (search-for-primes 100000) (list 100043 100019 100003))
  (check-equal? (search-for-primes 1000000) (list 1000037 1000033 1000003))

  ; Now, we verify that the running time for the test is increased by roughly sqrt(10)
  ; when "jumping" from one order of magnitude to the next. For this, we measure the time for
  ; computing the largest prime number smaller than 1e3, 1e4, 1e5 and 1e6 and compare them
  ; by using a helper function to measure the iteration count of the find-divisor procedure.

  (define (prime?-iterations n)
    (smallest-divisor-iterations n))

  (let* ([t1 (prime?-iterations 997)]
         [t2 (prime?-iterations 9973)]
         [t3 (prime?-iterations 99991)]
         [t4 (prime?-iterations 999983)])

    (check/= (* t1 (sqrt 10)) t2 1e-2)
    (check/= (* t2 (sqrt 10)) t3 1e-2)
    (check/= (* t3 (sqrt 10)) t4 1e-3)))
