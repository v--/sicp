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


(module+ test
  (require rackunit)
  (require support/measure-procedure)
  (require support/fuzzy-checks)

  (check-equal? (search-for-primes 1000) (list 1019 1013 1009))
  (check-equal? (search-for-primes 10000) (list 10037 10009 10007))
  (check-equal? (search-for-primes 100000) (list 100043 100019 100003))
  (check-equal? (search-for-primes 1000000) (list 1000037 1000033 1000003))

  ; Now, we empirically verify that the running time for the test is increased by roughly sqrt(10)
  ; when "jumping" from one order of magnitude to the next. For this, we measure the time for
  ; computing the largest prime number smaller than 1000, 10 000, 100 000 and 1 000 000 and compare them.
  ; The tests may fail because of fluctuations in computer performance.

  (let* ([t1 (measure-procedure prime? 997)]
         [t2 (measure-procedure prime? 9973)]
         [t3 (measure-procedure prime? 99991)]
         [t4 (measure-procedure prime? 999983)])

    (check/= (* t1 (sqrt 10)) t2 0.3)
    (check/= (* t2 (sqrt 10)) t3 0.3)
    (check/= (* t3 (sqrt 10)) t4 0.3)))
