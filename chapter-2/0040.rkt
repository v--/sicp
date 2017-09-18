#lang sicp

(require (only-in math prime?))
(require (only-in chapter-2/0033 accumulate))

; Exercise 2.40
;
; Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i, j)
; with 1 <= j < i <= n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.

; Solution
; Original definitions

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

; The required unique-pairs procedure
(define (unique-pairs n)
  (define (helper i)
    (map (lambda (j) (list i j))
         (enumerate-interval 1 (- i 1))))

  (flatmap helper (enumerate-interval 1 n)))

; The shortened prime-sum-pairs
(define (prime-sum-pairs-shortened n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(provide flatmap enumerate-interval)

(module+ test
  (require rackunit)

  ; Verify that unique-pairs works as expected
  (check-equal? (unique-pairs 1)
                (list))

  (check-equal? (unique-pairs 2)
                (list (list 2 1)))

  (check-equal? (unique-pairs 3)
                (list (list 2 1)
                      (list 3 1)
                      (list 3 2)))

  ; Verify that the shortened prime-sum-pairs produces the same results as the original
  (check-equal? (prime-sum-pairs 6)
                (prime-sum-pairs-shortened 6)))
