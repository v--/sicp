#lang sicp

(require support/picture-lang)
(require (only-in chapter-2/0049 outline))
(require (only-in chapter-2/0051 below beside))

; Exercise 2.44
;
; Define the procedure up-split used by corner-split. It is similar to right-split,
; except that it switches the roles of below and beside.

; Solution

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(module+ test
  (require rackunit)

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
