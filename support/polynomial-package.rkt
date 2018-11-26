#lang sicp

(require (only-in chapter-2/0080 install-zeros =zero?))
(require (only-in chapter-2/0056 variable? same-variable?))
(require support/operation-table)
(require support/generic-number-package)

; See section 2.5.3
; Definitions from the book
; Base
(define (make-poly variable term-list)
  (cons variable term-list))

(define (variable p) (car p))
(define (term-list p) (cdr p))

; Term lists
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define the-empty-termlist null)
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

; Terms
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

; Term addition
(define (add-terms L1 L2)
  (cond [(empty-termlist? L1) L2]
        [(empty-termlist? L2) L1]
        [else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (cond [(> (order t1) (order t2))
                   (adjoin-term
                     t1 (add-terms (rest-terms L1) L2))]
                  [(< (order t1) (order t2))
                   (adjoin-term
                     t2 (add-terms L1 (rest-terms L2)))]
                  [else
                    (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1)
                                 (rest-terms L2)))]))]))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

; Term multiplication
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ([t2 (first-term L)])
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

; Bindings
(define (install-base-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (install-polynomial-package)
  (install-base-polynomial-package)
  (install-zeros)
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(provide the-empty-termlist empty-termlist?
         first-term rest-terms make-term order coeff
         install-polynomial-package make-polynomial
         variable term-list
         =zero?)
