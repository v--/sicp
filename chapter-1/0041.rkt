#lang sicp

; Exercise 1.41
;
; Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies
; the original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then (double inc)
; should be a procedure that adds 2. What value is returned by
;
; (((double (double double)) inc) 5)

; Solution

(define (double proc)
  (lambda (x) (proc (proc x))))

(module* test #f
  (require rackunit)

  (check-equal? ((double inc) 1) 3)
  (check-equal? ((double dec) 3) 1)

  ; We'll use the notation f^n for n-fold repeated application of f.
  ; double can then be more conveniently written as double(f) = x -> f^2(x).
  ; Repeated application of double would then yield double^n(f) = x -> (f^(2^n))(x),
  ; because each repeated application of double simply doubles the number of applications of f.
  ;
  ; Using this notation, the desired Scheme expression
  ;   (((double (double double)) inc) 5)
  ; can be rewritten in informal notation as
  ;   ((double^2(double))(inc))(5)
  ; and then reduced to
  ;   ((x -> double^4(x))(inc))(5),
  ;   (double^4(inc))(5),
  ;   (inc^(2^4))(5),
  ;   (inc^16)(5),
  ; which simply equals 5 + 16 = 21.

  (check-equal? (((double (double double)) inc) 5) 21))
