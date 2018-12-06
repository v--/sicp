#lang sicp

(require support/operation-table)
(require support/generic-number-package)
(require (only-in support/polynomial-package coeff order make-term))

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

(define (make-term-list-dense-raw . terms)
  (if (null? terms)
      null
      (adjoin-term-dense (car terms)
                         (apply make-term-list-dense-raw (cdr terms)))))

(define (adjoin-term-dense term term-list)
  (let ([c (coeff term)]
        [o (order term)]
        [n (length term-list)])
    (cond [(=zero? c) term-list]
          [(> o n) (adjoin-term-dense term
                                      (cons (get 'zero (type-tag c)) term-list))]
          [else (cons c term-list)])))

(define (first-term-dense term-list)
  (cond [(null? term-list) (make-term 0 0)]
        [(=zero? (car term-list)) (first-term-dense (cdr term-list))]
        [else (make-term (dec (length term-list))
                         (car term-list))]))

(define (rest-terms-dense term-list)
  (cond [(empty-termlist?-dense term-list) (make-term-list-dense-raw)]
        [(empty-termlist?-dense (cdr term-list)) (make-term-list-dense-raw)]
        [(=zero? (car term-list)) (rest-terms-dense (cdr term-list))]
        [else (cdr term-list)]))

(define (empty-termlist?-dense term-list)
  (or (null? term-list)
      (and (=zero? (car term-list))
           (empty-termlist?-dense (cdr term-list)))))

(provide make-term-list-dense-raw adjoin-term-dense first-term-dense rest-terms-dense empty-termlist?-dense)

(module+ test
  (require rackunit)

  (void (install-generic-numbers-package))
  (void (install-zeros))
  (void (install-equ?))

  (check-true (empty-termlist?-dense
                (make-term-list-dense-raw (make-term 3 0))))

  (check same-term?
         (first-term-dense (make-term-list-dense-raw (make-term 3 3)))
         (make-term 3 3))

  (check-true (empty-termlist?-dense
                (rest-terms-dense
                  (make-term-list-dense-raw
                    (make-term-list-dense-raw (make-term 3 3))))))

  (check same-term?
         (first-term-dense
           (make-term-list-dense-raw (make-term 3 3) (make-term 1 3)))
         (make-term 3 3))

  (check same-term?
         (first-term-dense
           (rest-terms-dense
            (make-term-list-dense-raw (make-term 3 3) (make-term 1 3))))
         (make-term 1 3)))
