#lang sicp

(require support/picture-lang)
(require (only-in chapter-2/0049 wave))
(require (only-in chapter-2/0050 rotate270))

; Exercise 2.50
;
; Define the below operation for painters. Below takes two painters as arguments. The resulting
; painter, given a frame, draws with the first painter in the bottom of the frame and with
; the second painter in the top. Define below in two different ways - first by writing a procedure
; that is analogous to the beside procedure given above, and again in terms of beside and suitable
; rotation operations (from exercise 2.50).

; Solution
; Original definitions

(define (beside painter1 painter2)
  (let ([split-point (make-vect 0.5 0.0)])
    (let ([paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0))]
          [paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))])
      (lambda (frame)
        (matrix-or (paint-left frame)
                   (paint-right frame))))))

; See support/picture-lang.rkt for an explanation of why matrix-or is necessary
(define (below-1 painter1 painter2)
  (let ([paint-upper
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             (make-vect 0.0 0.5))]
        [paint-lower
          (transform-painter painter2
                             (make-vect 0.0 0.5)
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0))])
    (lambda (frame)
      (matrix-or (paint-upper frame)
                 (paint-lower frame)))))

(define (below-2 painter1 painter2)
  (rotate270 (beside (rotate270 painter1) (rotate270 painter2))))

(provide beside (rename-out [below-1 below]))

(module+ test
  (require rackunit)

  (define expected (string-join (list
                                  "      ####       "
                                  "     ### ###     "
                                  "###  ##   ##     "
                                  "## ####   #####  "
                                  "  ##         ####"
                                  "    #       #  ##"
                                  "    #  ###  #    "
                                  "   ##  # #  ##   "
                                  "   ###########   "
                                  "     ### ###     "
                                  "###  ##   ##     "
                                  "## ####   #####  "
                                  "  ##         ####"
                                  "    #       #  ##"
                                  "    #  ###  #    "
                                  "   ##  # #  ##   "
                                  "   ##### #####   "
                                  )
                                "\n"))

  (check-equal? (matrix->string ((below-1 wave wave) default-frame))
                expected)

  (check-equal? (matrix->string ((below-2 wave wave) default-frame))
                expected))
