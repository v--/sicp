#lang sicp

(require (only-in chapter-2/0067 leaf? symbol-leaf left-branch right-branch make-code-tree make-leaf decode symbols))

; Exercise 2.68
;
; The encode procedure takes as arguments a message and a tree and produces the list of bits that
; gives the encoded message.

(define (encode message tree)
  (if (null? message)
      null
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a
; given symbol according to a given tree. You should design encode-symbol so that it signals an
; error if the symbol is not in the tree at all. Test your procedure by encoding the result you
; obtained in exercise 2.67 with the sample tree and seeing whether it is the same as the original
; sample message.

; Solution

(define (encode-symbol symbol tree)
  (define (encode-symbol-iter subtree path)
    (if (leaf? subtree)
        (if (eq? symbol (symbol-leaf subtree))
            path
            null)

        (let ([left-path (encode-symbol-iter (left-branch subtree)
                                             (append path '(0)))])
          (if (null? left-path)
              (encode-symbol-iter (right-branch subtree)
                                  (append path '(1)))
              left-path))))

  (let ([encoded (encode-symbol-iter tree null)])
    (if (null? encoded)
        (error "bad symbol" symbol)
        encoded)))

(provide encode)

(module+ test
  (require rackunit)

  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                      (make-leaf 'B 2)
                      (make-code-tree (make-leaf 'D 1)
                                      (make-leaf 'C 1)))))

  (define sample-text '(A D A B B C A))

  (check-equal? (decode (encode sample-text sample-tree) sample-tree)
                sample-text))
