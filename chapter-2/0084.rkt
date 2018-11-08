#lang sicp

(require support/operation-table)
(require support/generic-number-package)

(require (only-in chapter-2/0083 install-raise-procedures raise))

; Exercise 2.84
;
; Using the raise operation of exercise 2.83, modify the apply-generic procedure so that it coerces
; its arguments to have the same type by the method of successive raising, as discussed in this
; section. You will need to devise a way to test which of two types is higher in the tower. Do this
; in a manner that is "compatible" with the rest of the system and will not lead to problems in
; adding new levels to the tower.

; Solution
; As in exercise 2.83, we will use the implemented types [scheme-number, rational, complex] rather
; than [interger, rational, real, compex].

(define (is-type-higher? type-a type-b)
  (or (and (eq? type-a 'complex) (eq? type-b 'rational))
      (and (eq? type-a 'rational) (eq? type-b 'scheme-number))))

(define (get-highest-type types)
  (define (get-highest-type-impl remaining-types currently-highest)
    (if (null? remaining-types)
        currently-highest
        (let ([current-type (car remaining-types)])
          (get-highest-type-impl (cdr remaining-types)
                                 (if (is-type-higher? current-type currently-highest)
                                     current-type
                                     currently-highest)))))

  (get-highest-type-impl types 'scheme-number))

(define (raise-to-type type value)
  (let ([value-type (type-tag value)])
    (cond [(eq? type value-type) value]
          [(is-type-higher? type value-type) (raise-to-type type (raise value))]
          [else (error "Cannot raise type" value-type "into a lower type" type)])))

(define (apply-generic-upcast op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          ; The modification starts here
          (let ([highest-type (get-highest-type type-tags)])
            (let ([coerced-proc (get op (map (lambda (type) highest-type) type-tags))])
                  (if coerced-proc
                      (apply coerced-proc (map (lambda (value) (contents (raise-to-type highest-type value))) args))
                      ; The modification ends here
                      (error
                        "No method for these types -- APPLY-GENERIC"
                        (list op type-tags)))))))))

(provide apply-generic-upcast)

(module+ test
  (require rackunit rackunit/text-ui)

  (void (install-generic-numbers-package))
  (void (install-raise-procedures))

  (check-equal? (apply-generic-upcast 'add
                                        (make-scheme-number 1)
                                        (make-scheme-number 2))
                (make-scheme-number 3))

  (check-equal? (apply-generic-upcast 'add
                                        (make-scheme-number 1)
                                        (make-rational 1 2))
                (make-rational 3 2))

  )
