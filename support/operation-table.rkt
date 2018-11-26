#lang racket

; See section 2.4 and 2.5

(define table (make-hash))
(define coercion-table (make-hash))

(define (put op type proc)
  (hash-set! table (list op type) proc))

(define (put-coercion src dest proc)
  (hash-set! coercion-table (list src dest) proc))

(define (get op type)
  (hash-ref table (list op type) #f))

(define (get-coercion src dest)
  (hash-ref coercion-table (list src dest) #f))

; Definitions from the book
(define (attach-tag-original type-tag contents)
  (cons type-tag contents))

(define (type-tag-original datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents-original datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; Definitions that are compatible with exercise 2.78 and section 2.5.3
(define (type-tag datum)
  (if (number? datum)
      'scheme-number
      (type-tag-original datum)))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (attach-tag-original type-tag contents)))

(define (contents datum)
  (if (number? datum)
      datum
      (contents-original datum)))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(provide put get put-coercion get-coercion attach-tag type-tag contents apply-generic)
