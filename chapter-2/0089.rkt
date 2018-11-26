#lang sicp

(require support/operation-table)
(require support/generic-number-package)
(require support/polynomial-package)

(require (only-in chapter-2/0079 install-equ?))
(require (only-in chapter-2/0080 install-zeros =zero?))
(require (only-in chapter-2/0087 same-term?))

; Exercise 2.87
;
; Define procedures that implement the term-list representation described above as appropriate for
; dense polynomials.

; Solution
; We still assume that new terms can only have orders that are strictly greater than the order of the
; polynomials whose terms they get adjoined to.
(define (adjoin-term-dense term term-list)
  (let ([c (coeff term)]
        [o (order term)]
        [n (length term-list)])
    (cond [(=zero? c) term-list]
          [(> o n) (adjoin-term-dense term
                                      (cons (get 'zero (type-tag c)) term-list))]
          [else (cons c term-list)])))

(define (first-term-dense term-list)
  (make-term (dec (length term-list))
             (car term-list)))

(define (rest-terms-dense term-list)
  (cond [(empty-termlist-dense? term-list) the-empty-termlist]
    [(empty-termlist-dense? (cdr term-list)) the-empty-termlist]
    [(=zero? (cadr term-list)) (rest-terms-dense (cdr term-list))]
    [else (cdr term-list)]))

(define (empty-termlist-dense? term-list) (null? term-list))

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-equ?))
  (void (install-zeros))

  (check-true (empty-termlist-dense?
                (adjoin-term-dense (make-term 3 0) the-empty-termlist)))

  (check same-term?
         (first-term-dense (adjoin-term-dense (make-term 3 3) the-empty-termlist))
         (make-term 3 3))

  (check-true (empty-termlist-dense?
                (rest-terms-dense (adjoin-term-dense (make-term 3 3) the-empty-termlist))))

  (check same-term?
         (first-term-dense (rest-terms-dense (adjoin-term-dense (make-term 3 3) (adjoin-term-dense (make-term 1 3) the-empty-termlist))))
         (make-term 1 3)))
