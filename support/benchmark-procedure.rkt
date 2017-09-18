#lang racket

(define (benchmark-procedure #:iterations [iterations 1e4] procedure . args)
  (define (with-args)
    (apply procedure args))

  (define (benchmark-procedure-iter i)
    (with-args)
    (if (= i iterations)
        iterations
        (benchmark-procedure-iter (+ i 1))))

  (let ([start (current-milliseconds)])
    (benchmark-procedure-iter 1)
    (/ (- (current-milliseconds) start) iterations)))

(provide benchmark-procedure)
