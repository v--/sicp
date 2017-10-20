#lang sicp

(require support/picture-lang)
(require (only-in chapter-2/0045 up-split right-split))
(require (only-in chapter-2/0049 outline wave))
(require (only-in chapter-2/0050 flip-horiz))
(require (only-in chapter-2/0051 below beside))

; Exercise 2.52
;
; Make changes to the square limit of wave shown in figure 2.9 by working at each of the levels
; described above. In particular:
;   a. Add some segments to the primitive wave painter of exercise  2.49 (to add a smile, for example).
;   b. Change the pattern constructed by corner-split (for example, by using only one copy
; of the up-split and right-split images instead of two).
;   c. Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern.
; (For example, you might make the big Mr. Rogers look outward from each corner of the square.)

; Solution
; Original definitions

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ([top (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; a. Smile and wave, boys!
(define smile (segments->painter (list (make-segment (make-vect 0.40 0.20)
                                                     (make-vect 0.50 0.25))
                                       (make-segment (make-vect 0.50 0.25)
                                                     (make-vect 0.60 0.20)))))

(define (wave-with-smile frame)
  (matrix-or (wave frame)
             (smile frame)))

; b. Simplified corner-split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([top-left (up-split painter (- n 1))]
            [bottom-right (right-split painter (- n 1))]
            [corner (corner-split painter (- n 1))])
        (beside (below painter top-left)
                (below bottom-right corner)))))

; b. Modified square-limit

(define tile (square-of-four flip-horiz
                             identity
                             (compose flip-horiz flip-vert)
                             flip-vert))

(define (square-limit painter n)
  (let ([quarter (corner-split painter n)])
    (tile quarter)))

(module+ test
  (require rackunit)

  (check-equal? (matrix->string (wave-with-smile default-frame))
                (string-join (list
                               "       ##        "
                               "      ## ##      "
                               "     ##   ##     "
                               "     #     #     "
                               "###  #######     "
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
                             "\n"))

  (check-equal? (matrix->string ((corner-split outline 2) default-frame))
                (string-join (list
                               "#################"
                               "#       #   #   #"
                               "#       #   #   #"
                               "#       #   #   #"
                               "#       #   #####"
                               "#       #   #   #"
                               "#       #   #   #"
                               "#       #   #   #"
                               "#################"
                               "#       #   #   #"
                               "#       #   #   #"
                               "#       #   #   #"
                               "#################"
                               "#   #   #   #   #"
                               "#   #   #   #   #"
                               "#   #   #   #   #"
                               "#################"
                               )
                             "\n"))

  (check-equal? (matrix->string ((square-limit outline 2) default-frame))
                (string-join (list
                               "#################"
                               "# # # # # # # # #"
                               "#################"
                               "# # #   #   # # #"
                               "#################"
                               "# # #   #   # # #"
                               "### #   #   # ###"
                               "# # #   #   # # #"
                               "#################"
                               "# # #   #   # # #"
                               "### #   #   # ###"
                               "# # #   #   # # #"
                               "#################"
                               "# # #   #   # # #"
                               "#################"
                               "# # # # # # # # #"
                               "#################"
                               )
                             "\n")))
