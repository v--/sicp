#lang sicp

(require (only-in chapter-2/0068 encode))
(require (only-in chapter-2/0069 generate-huffman-tree))

; Exercise 2.70
;
; The following eight-symbol alphabet with associated relative frequencies was designed to
; efficiently encode the lyrics of 1950s rock songs. (Note that the "symbols" of an "alphabet" need
; not be individual letters.)
;
; A     2   NA    16
; BOOM  1   SHA   3
; GET   2   YIP   9
; JOB   2   WAH   1
; Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree, and use encode
; (exercise 2.68) to encode the following message:
;
; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom
;
; How many bits are required for the encoding? What is the smallest number of bits that would be
; needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?

; Solution
; The variable-length encoding bit length of the song is 87, while the fixed-length encoding bit
; length is 3 * 36 = 108.

(module+ test
  (require rackunit)

  (define alphabet-pairs (list '(A 2)
                               '(BOOM 1)
                               '(GET 2)
                               '(JOB 2)
                               '(NA 16)
                               '(SHA 3)
                               '(YIP 9)
                               '(WAH 1)))

  (define song '(GET A JOB
                 SHA NA NA NA NA NA NA NA NA
                 GET A JOB
                 SHA NA NA NA NA NA NA NA NA
                 WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                 SHA BOOM))

  (define tree (generate-huffman-tree alphabet-pairs))

  (check-equal? (length (encode song tree))
                87))
