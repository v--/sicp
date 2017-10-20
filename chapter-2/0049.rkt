#lang sicp

(require support/picture-lang)

; Exercise 2.49
;
; Use segments->painter to define the following primitive painters:
;   a. The painter that draws the outline of the designated frame.
;   b. The painter that draws an "X" by connecting opposite corners of the frame.
;   c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;   d. The wave painter.

; Solution

; A wrapper around segments->painter that creates a painter out of a list of vectors.
; Each two consecutive vectors are connected with lines. The last vector is linked to the first one.
(define (vectors->painter vectors)
  (define (iter current-vector vectors result)
    (if (null? vectors)
        result
        (let ([next-vector (car vectors)]
              [rest (cdr vectors)])
          (iter next-vector rest (cons (make-segment current-vector next-vector) result)))))

  (let ([reversed-segments (iter (car vectors) (cdr vectors) null)])
    (segments->painter (cons (make-segment (car vectors)
                                           (end-segment (car reversed-segments)))
                             reversed-segments))))

(define outline (vectors->painter (list (make-vect 0 0)
                                        (make-vect 0 1)
                                        (make-vect 1 1)
                                        (make-vect 1 0))))

(define saltire (segments->painter (list (make-segment (make-vect 0 0)
                                                       (make-vect 1 1))
                                         (make-segment (make-vect 1 0)
                                                       (make-vect 0 1)))))

(define diamond (vectors->painter (list (make-vect 0.0 0.5)
                                        (make-vect 0.5 1.0)
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.5 0.0))))

(define wave (vectors->painter (list (make-vect 0.00 0.20)
                                     (make-vect 0.00 0.35)
                                     (make-vect 0.10 0.45)
                                     (make-vect 0.20 0.50)
                                     (make-vect 0.30 0.70)
                                     (make-vect 0.15 1.00)
                                     (make-vect 0.40 1.00)
                                     (make-vect 0.50 0.70)
                                     (make-vect 0.60 1.00)
                                     (make-vect 0.85 1.00)
                                     (make-vect 0.70 0.70)
                                     (make-vect 0.80 0.50)
                                     (make-vect 0.90 0.55)
                                     (make-vect 1.00 0.65)
                                     (make-vect 1.00 0.50)
                                     (make-vect 0.90 0.40)
                                     (make-vect 0.60 0.35)
                                     (make-vect 0.70 0.15)
                                     (make-vect 0.50 0.00)
                                     (make-vect 0.30 0.15)
                                     (make-vect 0.40 0.35)
                                     (make-vect 0.25 0.40)
                                     (make-vect 0.15 0.30))))

(provide outline saltire diamond wave)

(module+ test
  (require rackunit)

  (check-equal? (matrix->string (outline default-frame))
                (string-join (list
                               "#################"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#               #"
                               "#################"
                               )
                             "\n"))

  (check-equal? (matrix->string (saltire default-frame))
                (string-join (list
                               "#               #"
                               " #             # "
                               "  #           #  "
                               "   #         #   "
                               "    #       #    "
                               "     #     #     "
                               "      #   #      "
                               "       # #       "
                               "        #        "
                               "       # #       "
                               "      #   #      "
                               "     #     #     "
                               "    #       #    "
                               "   #         #   "
                               "  #           #  "
                               " #             # "
                               "#               #"
                               )
                             "\n"))

  (check-equal? (matrix->string (diamond default-frame))
                (string-join (list
                               "        #        "
                               "       # #       "
                               "      #   #      "
                               "     #     #     "
                               "    #       #    "
                               "   #         #   "
                               "  #           #  "
                               " #             # "
                               "#               #"
                               " #             # "
                               "  #           #  "
                               "   #         #   "
                               "    #       #    "
                               "     #     #     "
                               "      #   #      "
                               "       # #       "
                               "        #        "
                               )
                             "\n"))

  (check-equal? (matrix->string (wave default-frame))
                (string-join (list
                               "       ##        "
                               "      ## ##      "
                               "     ##   ##     "
                               "     #     #     "
                               "###  ##   ##     "
                               "#  #  #   #      "
                               "## ####   #####  "
                               " #             # "
                               "  ##         ## #"
                               "    #       #  ##"
                               "    #       #  ##"
                               "    #       #    "
                               "    #  ###  #    "
                               "    #  ###  #    "
                               "   ##  # #  ##   "
                               "   #   # #   #   "
                               "   ##### #####   "
                               )
                             "\n")))
