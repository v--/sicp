#lang sicp

(require support/operation-table)

; Exercise 2.78
;
; The internal procedures in the scheme-number package are essentially nothing more than calls to
; the primitive procedures +, -, etc. It was not possible to use the primitives of the language
; directly because our type-tag system requires that each data object have a type attached to it.
; In fact, however, all Lisp implementations do have a type system, which they use internally.
; Primitive predicates such as symbol? and number? determine whether data objects have particular
; types. Modify the definitions of type-tag, contents, and attach-tag from section 2.4.2 so that
; our generic system takes advantage of Scheme's internal type system. That is to say, the system
; should work as before except that ordinary numbers should be represented simply as Scheme numbers
; rather than as pairs whose car is the symbol scheme-number.

; Solution

(define (modified-type-tag datum)
  (if (number? datum)
      'scheme-number
      (type-tag datum)))

(define (modified-attach-tag type-tag contents)
  (if (number? contents)
      contents
      (attach-tag type-tag contents)))

(define (modified-contents datum)
  (if (number? datum)
      datum
      (contents datum)))

(module+ test
  (require rackunit)

  ; Make sure that add continues working for both scheme numbers and rational numbers
  ; Copy the apply-generic procedure but use the modified procedures
  (define (modified-apply-generic op . args)
    (let ([type-tags (map modified-type-tag args)])
      (let ([proc (get op type-tags)])
        (if proc
            (apply proc (map modified-contents args))
            (error
              "No method for these types -- APPLY-GENERIC"
              (list op type-tags))))))

  ; Define add for built-in numbers
  (put 'add '(scheme-number scheme-number)
    (lambda (x y) (modified-attach-tag 'scheme-number (+ x y))))

  ; Define add for rational numbers
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (modified-attach-tag 'rational (cons (/ n g) (/ d g)))))

  (define (add-rat x y)
    (define (numer x) (car x))
    (define (denom x) (cdr x))

    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (put 'add '(rational rational) add-rat)

  ; Define a generic add procedure
  (define (add x y) (modified-apply-generic 'add x y))

  (check-equal? (add 1 2)
                3)

  (check-equal? (add (make-rat 1 3) (make-rat 2 3))
                (make-rat 1 1))

  )
