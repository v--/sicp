#lang sicp

(require (only-in chapter-2/0063 entry left-branch right-branch make-tree))

; Exercise 2.66
;
; Implement the lookup procedure for the case where the set of records is structured as a binary
; tree, ordered by the numerical values of the keys.

; Solution

(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

(define (value record)
  (cdr record))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      null
      (let ([x (entry set-of-records)])
        (cond [(= given-key (key x)) x]
              [(< given-key (key x))
               (lookup given-key (left-branch set-of-records))]
              [else (lookup given-key (right-branch set-of-records))]))))

(module+ test
  (require rackunit)

  (define records (make-tree (make-record 1 "one")
                             (make-tree (make-record 0 "zero") null null)
                             (make-tree (make-record 3 "three")
                                        (make-tree (make-record 2 "two") null null)
                                        (make-tree (make-record 4 "four") null null))))

  (check-equal? (lookup -1 records)
                null)

  (check-equal? (value (lookup 0 records))
                "zero")

  (check-equal? (value (lookup 3 records))
                "three"))
