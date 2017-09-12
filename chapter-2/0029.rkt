#lang sicp

; Exercise 2.29
;
; A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod
; of a certain length, from which hangs either a weight or another binary mobile. We can represent
; a binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number) together with a structure,
; which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

; a. Write the corresponding selectors left-branch and right-branch, which return the branches
; of a mobile, and branch-length and branch-structure, which return the components of a branch.
;
; b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile.
;
; c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that
; applied by its top-right branch (that is, if the length of the left rod multiplied by the weight
; hanging from that rod is equal to the corresponding product for the right side) and if each of the
; submobiles hanging off its branches is balanced. Design a predicate that tests whether
; a binary mobile is balanced.
;
; d. Suppose we change the representation of mobiles so that the constructors are

(define (make-mobile-pairs left right)
  (cons left right))

(define (make-branch-pairs length structure)
  (cons length structure))

; How much do you need to change your programs to convert to the new representation?

; Solution

; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; b
(define (total-weight structure)
  (if (number? structure)
      structure
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))))

; c
(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? structure)
  (or (number? structure)
      (let ([l (left-branch structure)]
            [r (right-branch structure)])
        (and (= (torque l) (torque r))
             (balanced? (branch-structure l))
             (balanced? (branch-structure r))))))

; As for d - we would only need to change the right-branch and branch-structure selectors to use cdr.

(module+ test
  (require rackunit)

  (define flat-balanced (make-mobile (make-branch 1 4)
                                     (make-branch 2 2)))

  (define flat-unbalanced (make-mobile (make-branch 1 4)
                                       (make-branch 1 2)))

  (define nested-balanced (make-mobile (make-branch 1
                                                    (make-mobile (make-branch 1 4)
                                                                 (make-branch 2 2)))
                                       (make-branch 2
                                                    (make-mobile (make-branch 1 2)
                                                                 (make-branch 2 1)))))

  (define nested-unbalanced (make-mobile (make-branch 1
                                                      (make-mobile (make-branch 1 4)
                                                                   (make-branch 2 2)))
                                         (make-branch 1
                                                      (make-mobile (make-branch 1 2)
                                                                   (make-branch 2 1)))))

  ; Test total-weight
  (check-equal? (total-weight flat-balanced) 6)
  (check-equal? (total-weight flat-unbalanced) 6)
  (check-equal? (total-weight nested-balanced) 9)
  (check-equal? (total-weight nested-unbalanced) 9)

  ; Test balanced?
  (check-true (balanced? flat-balanced))
  (check-false (balanced? flat-unbalanced))
  (check-true (balanced? nested-balanced))
  (check-false (balanced? nested-unbalanced)))
