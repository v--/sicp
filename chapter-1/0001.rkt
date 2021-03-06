#lang sicp

; Exercise 1.1
;
; Below is a sequence of expressions. What is the result printed by the interpreter
; in response to each expression? Assume that the sequence is to be evaluated
; in the order in which it is presented.

; Solution

(module+ test
  (require rackunit)

  (check-equal? 10
                10)

  (check-equal? (+ 5 3 4)
                12)

  (check-equal? (- 9 1)
                8)

  (check-equal? (/ 6 2)
                3)

  (check-equal? (+ (* 2 4) (- 4 6))
                6)

  (define a 3) ; Not allowed in expressions
  (define b (+ a 1)) ; Not allowed in expressions

  (check-equal? (+ a b (* a b))
                19)

  (check-equal? (= a b)
                false)

  (check-equal? (if (and (> b a) (< b (* a b)))
                    b
                    a)
                4)

  (check-equal? (cond [(= a 4) 6]
                      [(= b 4) (+ 6 7 a)]
                      [else 25])
                16)

  (check-equal? (+ 2 (if (> b a) b a))
                6)

  (check-equal? (* (cond [(> a b) a]
                         [(< a b) b]
                         [else -1])
                   (+ a 1))
                16))
