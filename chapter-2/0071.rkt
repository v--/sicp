#lang sicp

(require (only-in chapter-2/0067 make-leaf make-code-tree))
(require (only-in chapter-2/0068 encode))
(require (only-in chapter-2/0069 generate-huffman-tree))

; Exercise 2.71
;
; Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of
; the symbols are 1, 2, 4, ..., 2n-1. Sketch the tree for n=5; for n=10. In such a tree (for general
; n) how many bits are required to encode the most frequent symbol? the least frequent symbol?

; Solution
; The following tree describes the Huffman tree for [(A, 1), (B, 2), (C, 4), (D, 8), (E, 16)].
; The case n=10 is completely analogous.
;
; ({A B C D E}, 31)
; ├──(E, 16)
; └──({A B C D}, 15)
;    ├──(D, 8)
;    └──({A B C}, 7)
;       ├──(C, 4)
;       └──({A B}, 3)
;          ├──(B, 2)
;          └──(A, 1)
;
; We can easily see that encoding the k-th most frequest symbol takes min(k, n-1) bits.

(module+ test
  (require rackunit)

  ; First, verify that the tree indeed has the described structure.
  (define pairs (list '(A 1)
                      '(B 2)
                      '(C 4)
                      '(D 8)
                      '(E 16)))

  (define tree
    (make-code-tree (make-leaf 'E 16)
                    (make-code-tree (make-leaf 'D 8)
                                    (make-code-tree (make-leaf 'C 4)
                                                    (make-code-tree (make-leaf 'B 2)
                                                                    (make-leaf 'A 1))))))

  (check-equal? (generate-huffman-tree pairs)
                tree)

  ; Verify the most frequent symbol is encoded using 1 bit.
  (check-equal? (length (encode '(E) tree))
                1)

  ; Verify the least frequent symbol is encoded using n-1 bits.
  (check-equal? (length (encode '(A) tree))
                (- (length pairs) 1)))
