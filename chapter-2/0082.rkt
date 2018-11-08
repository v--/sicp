#lang sicp

(require support/operation-table)
(require support/generic-number-package)

(require (only-in chapter-2/0040 flatmap))
(require (only-in chapter-2/0059 element-of-set?))

; Exercise 2.82
;
; Show how to generalize apply-generic to handle coercion in the general case of multiple arguments.
; One strategy is to attempt to coerce all the arguments to the type of the first argument, then to
; the type of the second argument, and so on. Give an example of a situation where this strategy
; (and likewise the two-argument version given above) is not sufficiently general. (Hint: Consider
; the case where there are some suitable mixed-type operations present in the table that will not
; be tried.)

; Solution
; We decided not to limit ourselves and provide a complete solution that includes mixed-type
; operations. This can easily be reduced to the case where all of the resulting types are identical
; by simplifying the variations procedure.
;
; We assume that we want to preserve the encapsulation of the operation storage (i.e. we do not want
; to provide a way to retrieve all defined variants of an operation). The simplest approach then is
; to attempt to convert each one of the arguments to the types of the others, skipping coercing
; arguments to their own type.

; Generate variations with repetition
(define (variations lst n)
  (cond [(= n 0) null]
        [(= n 1) (map (lambda (x) (list x)) lst)]
        [else (flatmap (lambda (x)
                         (map (lambda (seq)
                                (cons x seq))
                              (variations lst (- n 1))))
                       lst)]))

; A naive implementation without hashing
(define (distinct lst)
  (define (distinct-impl unique remaining)
    (cond [(null? remaining) unique]
          [(element-of-set? (car remaining) (cdr remaining))
           (distinct-impl unique (cdr remaining))]
          [else (distinct-impl (cons (car remaining) unique)
                               (cdr remaining))]))

  (distinct-impl null lst))

(define (can-be-coerced? args types)
  (if (null? args)
      #t
      (let ([src-type (type-tag (car args))]
            [dest-type (car types)])
        (if (or (eq? src-type dest-type) (get-coercion src-type dest-type))
            (can-be-coerced? (cdr args) (cdr types))
            #f))))

; We assume that this procedure is always called with types that the args can be coerced to.
(define (coerce args types)
  (if (null? args)
      null
      (let ([arg (car args)]
            [dest-type (car types)])
        (cons
          (if (eq? (type-tag arg) dest-type)
              arg
              ((get-coercion (type-tag arg) dest-type) arg))
          (coerce (cdr args) (cdr types))))))

(define (apply-generic-multiple op . args)
  (let ([type-tags (map type-tag args)])
    (let ([valid-coercions (filter (lambda (types) (and (can-be-coerced? args types)
                                                        (get op types)))
                                   (variations (distinct type-tags) (length args)))])
      (if (null? valid-coercions)
          (error "No method for these types"
                 (list op type-tags))
          (let ([types (car valid-coercions)])
            (apply (get op types) (map contents (coerce args types))))))))

(module+ test
  (require rackunit)

  ; First, verify that the variations procedure works
  (check-equal? (variations '(a b) 0)
                null)

  (check-equal? (variations '(a b) 1)
                (list '(a)
                      '(b)))

  (check-equal? (variations '(a b) 2)
                (list '(a a)
                      '(a b)
                      '(b a)
                      '(b b)))

  ; Verify that the distinct procedure works
  (check-equal? (sort (distinct (list 1 2 3)) <)
                (list 1 2 3))

  (check-equal? (sort (distinct (list 1 1 1)) <)
                (list 1))

  (check-equal? (sort (distinct (list 1 1 2 2 3)) <)
                (list 1 2 3))

  ; Verify that apply-generic-multiple works
  (void (install-generic-numbers-package))

  ; apply-generic-multiple works for three scheme numbers
  (put 'add '(scheme-number scheme-number scheme-number)
       (lambda args (make-scheme-number (apply + args))))

  (check-equal? (apply-generic-multiple 'add
                                        (make-scheme-number 1)
                                        (make-scheme-number 2)
                                        (make-scheme-number 3))
                (make-scheme-number 6))

  ; apply-generic-multiple upcasts scheme numbers to complex numbers
  (put 'add '(complex complex complex)
       (lambda args (foldl (get 'add '(complex complex)) (make-complex-from-real-imag 0 0) args)))

  (check-equal? (apply-generic-multiple 'add
                                        (make-scheme-number 1)
                                        (make-complex-from-real-imag 0 3)
                                        (make-scheme-number 2))
                (make-complex-from-real-imag 3 3)))
