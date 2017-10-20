#lang sicp

(require support/picture-lang)
(require (only-in chapter-2/0049 outline))
(require (only-in chapter-2/0051 below beside))

; Exercise 2.45
;
; right-split and up-split can be expressed as instances of a general splitting operation.
; Define a procedure split with the property that evaluating
;
; (define right-split (split beside below))
; (define up-split (split below beside))
;
; produces procedures right-split and up-split with the same behaviors as the ones already defined.

; Solution

(define (split a b)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (a painter (b smaller smaller)))))

  splitter)

(define right-split (split beside below))
(define up-split (split below beside))
(provide right-split up-split)

(module+ test
  (require rackunit)

  (check-equal? (matrix->string ((right-split outline 1) default-frame))
                (string-join (list
                               "#################"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #########"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#################"
                               )
                             "\n"))

  (check-equal? (matrix->string ((up-split outline 1) default-frame))
                (string-join (list
                               "#################"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#################"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#       #       #"
                               "#################"
                               )
                             "\n")))
