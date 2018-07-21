#lang sicp

(require (only-in chapter-2/0067 leaf? symbol-leaf left-branch right-branch))
(require (only-in chapter-2/0069 generate-huffman-tree))

; Exercise 2.72
;
; Consider the encoding procedure that you designed in exercise 2.68. What is the order of growth in
; the number of steps needed to encode a symbol? Be sure to include the number of steps needed to
; search the symbol list at each node encountered. To answer this question in general is difficult.
; Consider the special case where the relative frequencies of the n symbols are as described in
; exercise 2.71, and give the order of growth (as a function of n) of the number of steps needed to
; encode the most frequent and least frequent symbols in the alphabet.

; Solution
; We begin by noting that the encode-symbol procedure is simply a depth-first search that searches
; for the metching leaf node by starting from the root and then continuing left until it hits a leaf.
; The symbol lists are not searched unless we arrive at a leaf node, so the worst-case number of
; steps occurs in a complete binary tree, where we have 2(n-1) total steps if the required leaf is
; the rightmost leaf of the encoding tree. Note that we have a full binary tree of height n if we
; have 2^n symbols with equal frequencies. The best-case number of steps is 1 and occurs if the leaf
; is directly to the left of the root. This is the case if we try to encode the most frequently used
; symbol in the situation described in exercise 2.71. The worst-case complexity in the case of
; exercise 2.71 is the same as in the generic scenario.

; We can verify these claims by designing the encode-symbol-iterations procedure that is analogous
; to the one in exercise 2.68 but return the number of iterations instead of the encoded message.

(define (encode-symbol-iterations symbol tree)
  (define return-data cons)
  (define if-symbol-found car)
  (define iteration-count cdr)
  (define (encode-symbol-iter subtree iterations)
    (if (leaf? subtree)
        (return-data (eq? symbol (symbol-leaf subtree)) iterations)
        (let ([left-path (encode-symbol-iter (left-branch subtree) (+ iterations 1))])
          (if (if-symbol-found left-path)
              left-path
              (encode-symbol-iter (right-branch subtree) (+ (iteration-count left-path) 1))))))

  (iteration-count (encode-symbol-iter tree 0)))

(module+ test
  (require rackunit)

  (define n 8)
  (define full-tree (generate-huffman-tree (list '(A 1)
                                                 '(B 1)
                                                 '(C 1)
                                                 '(D 1)
                                                 '(E 1)
                                                 '(F 1)
                                                 '(G 1)
                                                 '(H 1))))

  ; Worst possible case
  (check-equal? (encode-symbol-iterations 'F full-tree)
                (* 2 (- n 1)))

  (define high-tree (generate-huffman-tree (list '(A 1)
                                                 '(B 2)
                                                 '(C 4)
                                                 '(D 8)
                                                 '(E 16)
                                                 '(F 32)
                                                 '(G 64)
                                                 '(H 128))))

  ; Best possible case
  (check-equal? (encode-symbol-iterations 'H high-tree)
                1)

  ; Worst case for linearized trees
  (check-equal? (encode-symbol-iterations 'A high-tree)
                (encode-symbol-iterations 'F full-tree)))
