#lang racket

(define (measure-procedure #:iterations [iterations 1e4] procedure . args)
  (define (with-args)
    (apply procedure args))

  (define (measure-procedure-iter i)
    (with-args)
    (if (= i iterations)
        iterations
        (measure-procedure-iter (+ i 1))))

  (let ([start (current-milliseconds)])
    (measure-procedure-iter 1)
    (/ (- (current-milliseconds) start) iterations)))

(provide measure-procedure)
