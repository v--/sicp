#lang sicp

(require support/picture-lang)
(require (only-in chapter-2/0049 wave))

; Exercise 2.50
;
; Define the transformation flip-horiz, which flips painters horizontally, and transformations
; that rotate painters counterclockwise by 180 degrees and 270 degrees.

; Solution

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(provide flip-horiz rotate180 rotate270)

(module+ test
  (require rackunit)

  (check-equal? (matrix->string ((flip-horiz wave) default-frame))
                (string-join (list
                               "        #        "
                               "      ## ##      "
                               "     ##   ##     "
                               "     #     #     "
                               "     ##   ##  ###"
                               "      #   #  #  #"
                               "  #####   #### ##"
                               " #             # "
                               "# ##         ##  "
                               "##  #       #    "
                               "##  #       #    "
                               "    #       #    "
                               "    #  ###  #    "
                               "    #  ###  #    "
                               "   ##  # #  ##   "
                               "   #   # #   #   "
                               "   ##### #####   "
                               )
                             "\n"))

  (check-equal? (matrix->string ((rotate180 wave) default-frame))
                (string-join (list
                               "   ##### #####   "
                               "   #   # #   #   "
                               "   ##  # #  ##   "
                               "    #  ###  #    "
                               "    #  ###  #    "
                               "    #       #    "
                               "##  #       #    "
                               "##  #       #    "
                               "# ##         ##  "
                               " #             # "
                               "  #####   #### ##"
                               "      #   #  #  #"
                               "     ##   ##  ###"
                               "     #     #     "
                               "     ##   ##     "
                               "      ## ##      "
                               "        #        "
                               )
                             "\n"))

  (check-equal? (matrix->string ((rotate270 wave) default-frame))
                (string-join (list
                               "    ###          "
                               "    # ##         "
                               "    #   #        "
                               "     ## #     ###"
                               "      #  ###### #"
                               "  ### #         #"
                               " ## ###         #"
                               "##          #####"
                               "#           ##   "
                               " #          #####"
                               " ## ###         #"
                               "  ### #         #"
                               "      #  ###### #"
                               "      # #     ###"
                               "      # #        "
                               "       # ##      "
                               "        ###      "
                               )
                             "\n")))
