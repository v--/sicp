#lang sicp

; Exercise 2.32
;
; We can represent a set as a list of distinct elements, and we can represent the set of all subsets
; of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that
; generates the set of subsets of a set and give a clear explanation of why it works:
;
; (define (subsets s)
;   (if (null? s)
;       (list null)
;       (let ([rest (subsets (cdr s))])
;         (append rest (map <??> rest)))))

; Solution

(define (subsets s)
  (define (transform item)
    (cons (car s) item))

  (if (null? s)
      (list null)
      (let ([rest (subsets (cdr s))])
        (append rest (map transform rest)))))

; The nested transform procedure prepends the car of s to all the existing subsets.
; When the recursion starts unwinding, the first step produces a subset list with only the empty set,
; the second step prepends 3 to all existing sets (i.e. only to the empty set, so we add (3) to the
; subsets), the third step prepends 2 to both the empty set and (3) and so on.

(module+ test
  (require rackunit)

  (check-equal? (subsets null)
                (list null))

  (check-equal? (subsets (list 1 2 3))
                (list (list)
                      (list 3)
                      (list 2)
                      (list 2 3)
                      (list 1)
                      (list 1 3)
                      (list 1 2)
                      (list 1 2 3))))
