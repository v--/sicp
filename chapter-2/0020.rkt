#lang sicp

; Exercise 2.20
;
; Use this notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,

(module+ test
  (require rackunit)

  (check-equal? (same-parity 1 2 3 4 5 6 7)
                (list 1 3 5 7))

  (check-equal? (same-parity 2 3 4 5 6 7)
                (list 2 4 6)))

; Solution

(define (matches-parity x y)
  (or (and (even? x) (even? y))
      (and (odd? x) (odd? y))))

(define (same-parity reference . values)
  (define (iter values result)
    (if (null? values)
        result
        (iter (cdr values) (if (matches-parity reference (car values))
                               (append result (list (car values)))
                               result))))

  (iter values (list reference)))
