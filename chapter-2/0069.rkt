#lang sicp

(require (only-in chapter-2/0067 make-leaf make-leaf-set make-code-tree weight))

; Exercise 2.69
;
; The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol
; appears in more than one pair) and generates a Huffman encoding tree according to the Huffman
; algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set
; of leaves. successive-merge is the procedure you must write, using make-code-tree to successively
; merge the smallest-weight elements of the set until there is only one element left, which is the
; desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find
; yourself designing a complex procedure, then you are almost certainly doing something wrong. You
; can take significant advantage of the fact that we are using an ordered set representation.)

; Solution

; This procedure is just a little modification of the one in 2.61
(define (adjoin-leaf-set x set)
  (if (null? set)
      (list x)
      (let ([smallest (car set)])
        (if (<= (weight x) (weight smallest))
            (cons x set)
            (cons smallest (adjoin-leaf-set x (cdr set)))))))

(define (successive-merge leaf-set)
  (cond [(null? leaf-set) null]
        [(null? (cdr leaf-set)) (car leaf-set)]
        [else (successive-merge (adjoin-leaf-set (make-code-tree (cadr leaf-set) (car leaf-set))
                                                 (cddr leaf-set)))]))

(provide generate-huffman-tree)

(module+ test
  (require rackunit)

  (check-equal? (generate-huffman-tree (list '(A 1)))
                (make-leaf 'A 1))

  (check-equal? (generate-huffman-tree (list '(A 2) '(B 1)))
                (make-code-tree (make-leaf 'A 2) (make-leaf 'B 1)))

  (define sample-pairs (list '(A 4)
                             '(B 2)
                             '(C 1)
                             '(D 1)))

  (define sample-tree
    (make-code-tree
      (make-leaf 'A 4)
      (make-code-tree
        (make-leaf 'B 2)
        (make-code-tree (make-leaf 'C 1)
                        (make-leaf 'D 1)))))

  (check-equal? (generate-huffman-tree sample-pairs)
                sample-tree))
