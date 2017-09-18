#lang sicp

(require (only-in chapter-1/0014 sum))
(require (only-in chapter-2/0040 enumerate-interval flatmap))

; Exercise 2.41
;
; Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than
; or equal to a given integer n that sum to a given integer s.

; Solution

(define (distinct-triples n)
  (define (triples-for-i i)
    (define (triples-for-j j)
      (define (triples-for-k k)
        (list i j k))
      (map triples-for-k (enumerate-interval (inc j) n)))
    (flatmap triples-for-j (enumerate-interval (inc i) n)))
  (flatmap triples-for-i (enumerate-interval 1 n)))

(define (n-sum-triples n s)
  (define (sum? x)
    (= (sum x) s))

  (filter sum? (distinct-triples n)))

(module+ test
  (require rackunit)

  ; Verify that distinct-triples works as expected
  (check-equal? (distinct-triples 2)
                (list))

  (check-equal? (distinct-triples 3)
                (list (list 1 2 3)))

  (check-equal? (distinct-triples 4)
                (list (list 1 2 3)
                      (list 1 2 4)
                      (list 1 3 4)
                      (list 2 3 4)))

  ; Also verify n-sum-tuples
  (check-equal? (n-sum-triples 3 6)
                (list (list 1 2 3)))

  (check-equal? (n-sum-triples 10 8)
                (list (list 1 2 5)
                      (list 1 3 4))))
