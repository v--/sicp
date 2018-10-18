#lang racket

; See section 2.4 and 2.5

(define table (make-hash))

(define (put op type proc)
  (hash-set! table (list op type) proc))

(define (get op type)
  (hash-ref table (list op type) #f))

; Definitions from the book
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(provide put get attach-tag type-tag contents apply-generic)
