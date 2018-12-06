#lang sicp

(require support/operation-table)
(require support/generic-number-package)
(require support/polynomial-package)

(require (only-in chapter-2/0079 equ? install-equ?))
(require (only-in chapter-2/0087 install-polynomial-equ?))
(require (only-in chapter-2/0089 make-term-list-dense-raw adjoin-term-dense first-term-dense rest-terms-dense empty-termlist?-dense))

; Exercise 2.90
;
; Suppose we want to have a polynomial system that is efficient for both sparse and dense polynomials.
; One way to do this is to allow both kinds of term-list representations in our system. The situation
; is analogous to the complex-number example of section 2.4, where we allowed both rectangular and
; polar representations. To do this we must distinguish different types of term lists and make the
; operations on term lists generic. Redesign the polynomial system to implement this generalization.
; This is a major effort, not a local change.

; Solution
; We use the extended polynomial package from the book that supports generic term lists.

(define (make-term-list-dense . terms)
  (attach-tag 'dense (apply make-term-list-dense-raw terms)))

(define (install-dense-term-list-package)
  (define (tag p) (attach-tag 'dense p))

  (put 'empty-termlist? '(dense) empty-termlist?-dense)
  (put 'first-term '(dense) first-term-dense)
  (put 'rest-terms '(dense)
       (lambda (term-list) (tag (rest-terms-dense term-list))))

  (put 'adjoin-term '(dense)
       (lambda (term-list)
         (lambda (term) (tag (adjoin-term-dense term term-list)))))

  'done)

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-polynomial-package))
  (void (install-equ?))
  (void (install-polynomial-equ?))
  (void (install-dense-term-list-package))

  (check-true (empty-termlist? (make-term-list-dense)))

  (check equ?
      (make-polynomial 'x
                       (make-term-list-dense (make-term 2 2) (make-term 1 1)))

      (make-polynomial 'x
                       (make-term-list-sparse (make-term 2 2) (make-term 1 1)))))

