#lang sicp

(require (only-in chapter-2/0033 accumulate))

; Exercise 2.39
;
; The accumulate procedure is also known as fold-right, because it combines the first element
; of the sequence with the result of combining all the elements to the right. There is also
; a fold-left, which is similar to fold-right, except that it combines elements working
; in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; What are the values of
;
; (fold-right / 1 (list 1 2 3))
; (fold-left / 1 (list 1 2 3))
; (fold-right list null (list 1 2 3))
; (fold-left list null (list 1 2 3))
;
; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.

; Solution
; Original definitions
(define fold-right accumulate)
(provide fold-left fold-right)

(module+ test
  (require rackunit)

  ; We use substitution to closer inspect the workings of fold-left and fold-right
  ; We begin with (fold-right / 1 (list 1 2 3))
  (define-simple-check (check-3/2? x)
    (= x 3/2))

  (check-3/2? (fold-right /
                          1
                          (list 1 2 3)))

  (check-3/2? (fold-right /
                          3/1
                          (list 1 2)))

  (check-3/2? (fold-right /
                          2/3
                          (list 1)))

  (check-3/2? (fold-right /
                          3/2
                          (list)))

  ; We proceed with (fold-left / 1 (list 1 2 3))
  (define-simple-check (check-1/6? x)
    (= x 1/6))

  (check-1/6? (fold-left /
                         1
                         (list 1 2 3)))

  (check-1/6? (fold-left /
                         1/1
                         (list 2 3)))

  (check-1/6? (fold-left /
                         1/2
                         (list 3)))

  (check-1/6? (fold-left /
                         1/6
                         (list)))

  ; Now (fold-right list null (list 1 2 3))
  (define-simple-check (check-foldr-tree x)
    (equal? x (list 1 (list 2 (list 3 null)))))

  (check-foldr-tree (fold-right list
                                null
                                (list 1 2 3)))

  (check-foldr-tree (fold-right list
                                (list 3 null)
                                (list 1 2)))

  (check-foldr-tree (fold-right list
                                (list 2 (list 3 null))
                                (list 1)))

  (check-foldr-tree (fold-right list
                                (list 1 (list 2 (list 3 null)))
                                (list)))

  ; Finally, (fold-left list null (list 1 2 3))
  (define-simple-check (check-foldl-tree x)
    (equal? x (list (list (list null 1) 2) 3)))

  (check-foldl-tree (fold-left list
                               null
                               (list 1 2 3)))

  (check-foldl-tree (fold-left list
                               (list null 1)
                               (list 2 3)))

  (check-foldl-tree (fold-left list
                               (list (list null 1) 2)
                               (list 3)))

  (check-foldl-tree (fold-left list
                               (list (list (list null 1) 2) 3)
                               (list)))

  ; The two procedures should give equal results if the operation is associative and commutative,
  ; for example integer addition:

  (check-equal? (fold-left  + 0 (list 1 2 3))
                (fold-right + 0 (list 1 2 3)))

  ; The / and list procedures from the above examples are neither associative nor commutative.
  ; We can demonstrate, however, that neither associativity nor commutativity alone suffice.

  (define (only-commutative x y)
    (abs (- x y)))

  (check-not-equal? (fold-left  only-commutative 0 (list 1 2 3))
                    (fold-right only-commutative 0 (list 1 2 3)))

  (define (only-associative x y)
    (+ (* x y) 1))

  (check-not-equal? (fold-left  only-associative 0 (list 1 2 3))
                    (fold-right only-associative 0 (list 1 2 3))))
