#lang sicp

(require (only-in chapter-2/0033 accumulate))

; Exercise 2.35
;
; Redefine count-leaves from section 2.2.2 as an accumulation:
;
; (define (count-leaves t)
;   (accumulate <??> <??> (map <??> <??>)))

; Solution

(define (count t)
  (cond [(null? t) 0]
        [(pair? t) (count-leaves t)]
        [else 1]))

(define (count-leaves t)
  (accumulate + 0 (map count t)))

(module+ test
  (require rackunit)

  (define tree (list 1 (list 2 (list 3 4))))

  (check-equal? (count-leaves tree) 4))
