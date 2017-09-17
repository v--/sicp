#lang sicp

(require (only-in chapter-2/0033 accumulate))
(require (only-in chapter-2/0036 accumulate-n))

; Exercise 2.37
;
; Suppose we represent vectors v = (v_i) as sequences of numbers, and matrices m = (m_(i,j))
; as sequences of vectors (the rows of the matrix). For example, the matrix
;   1 2 3 4
;   4 5 6 6
;   6 7 8 9
; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation,
; we can use sequence operations to concisely express the basic matrix and vector operations.
; These operations (which are described in any book on matrix algebra) are the following:
;
; (dot-product v w)        returns the sum sum_i (v_i * w_i)
; (matrix-*-vector m v)    returns the vector t, where t_i = sum_j (m_(i,j) * v_i)
; (matrix-*-matrix m n)    returns the matrix p, where p_(i,j) = sum_k (m_(i,k) * n_(k,j))
; (transpose m)            returns the matrix n, where n_(i,j) = m_(j,i)
;
; We can define the dot product as

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing the other matrix
; operations. (The procedure accumulate-n is defined in exercise 2.36.)
;
; (define (matrix-*-vector m v)
;   (map <??> m))
;
; (define (transpose mat)
;   (accumulate-n <??> <??> mat))
;
; (define (matrix-*-matrix m n)
;   (let ([cols (transpose n)])
;     (map <??> m)))

; Solution

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(module+ test
  (require rackunit)

  (check-equal? (dot-product (list 1 2)
                             (list 3 4))
                11)

  (check-equal? (matrix-*-vector (list (list 1 2)
                                       (list 3 4))
                                 (list 5 6))
                (list 17 39))

  (check-equal? (transpose (list (list 1 2)
                                 (list 3 4)))
                (list (list 1 3)
                      (list 2 4)))

  (check-equal? (matrix-*-matrix (list (list 1 2)
                                       (list 3 4))
                                 (list (list 5 6)
                                       (list 7 8)))
                (list (list 19 22)
                      (list 43 50))))
