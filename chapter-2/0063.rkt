#lang sicp

; Exercise 2.63
;
; a. Do the two procedures produce the same result for every tree? If not, how do the results differ?
; What lists do the two procedures produce for the trees in figure 2.16?
;
; b. Do the two procedures have the same order of growth in the number of steps required to convert
; a balanced tree with n elements to a list? If not, which one grows more slowly?

(define (tree->list-1 tree)
  (if (null? tree)
      null
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree null))

; Solution
; Original definitions

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; The trees from figure 2.16

(define tree1 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 null null)
                                    (make-tree 5 null null))

                         (make-tree 9
                                    null
                                    (make-tree 11 null null))))

(define tree2 (make-tree 3
                         (make-tree 1 null null)
                         (make-tree 7
                                    (make-tree 5 null null)
                                    (make-tree 9
                                               null
                                               (make-tree 11 null null)))))

(define tree3 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 null null)
                                    null)
                         (make-tree 9
                                    (make-tree 7 null null)
                                    (make-tree 11 null null))))

; a. We are going to visualize the call trees for the two procedures tree->list-1
; and tree->list-2 using the following notation:
; 1) A stands for the tree->list-1 procedure and B stands for the copy-to-list
; procedure inside tree->list-2.
; 2) [] represent matlab-style arrays with the following two rules:
;   2.1) [null] = []
;   2.2) [[x]] = [x]
; 3) {} represent tree triples
;
; The evaluation tree for tree->list-1 with tree1 then looks like this:
; A({7 {3 1 5} {9 null 11}})
; =
; [A({3 1 5}) 7 A({9 null 11})]
; =
; [[A({1}) 3 A({5})] 7 [A(null) 9 A({11})]]
; =
; [[[A(null) 1 A(null)] 3 [A(null) 5 A(null)]] 7 [A(null) 9 [A(null) 11 A(null)]]]
; =
; [[[1] 3 [5]] 7 [9 [11]]]
; =
; [1 3 5 7 9 11]
;
; Compare to the evaluation tree for tree->list-2:
; B({7 {3 1 5} {9 null 11}}, [])
; =
; B({3 1 5}, [7 B({9 null 11}, [])])
; =
; B({1}, [3 B({5}, [7 B(null, [9 B({11}, [])])])])
; =
; B(null, [1 B(null, [3 B(null, [5 B(null, [7 B(null, [9 B(null, [11, B(null, [])])])])])])])
; =
; [1 [3 [5 [7 [9 [11 []]]]]]]
; =
; [1 3 5 7 9 11]
;
; b. We see that tree->list-1 is conceptually simpler while tree->list-2 produces a simpler,
; "linearized" evaluation tree. Both apply themselves O(n) times. Also, tree->list-1 requires about
; twice as much auxilary storage.

(provide entry left-branch right-branch make-tree
         (rename-out [tree->list-1 tree->list]))

(module+ test
  (require rackunit)

  (check-equal? (tree->list-1 tree1)
                '(1 3 5 7 9 11))

  (check-equal? (tree->list-1 tree1)
                (tree->list-2 tree1))

  (check-equal? (tree->list-1 tree2)
                (tree->list-2 tree2))

  (check-equal? (tree->list-1 tree3)
                (tree->list-2 tree3)))
