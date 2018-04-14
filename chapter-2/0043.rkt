#lang sicp

(require (only-in chapter-2/0040 enumerate-interval flatmap))
(require (only-in chapter-2/0042 adjoin-position empty-board safe?))

; Exercise 2.43
;
; Louis Reasoner is having a terrible time doing exercise 2.42. His queens procedure seems to work,
; but it runs extremely slowly. (Louis never does manage to wait long enough for it to solve even
; the 6×6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged
; the order of the nested mappings in the flatmap, writing it as
;
; (flatmap
;   (lambda (new-row)
;     (map (lambda (rest-of-queens)
;            (adjoin-position new-row k rest-of-queens))
;          (queen-cols (- k 1))))
;   (enumerate-interval 1 board-size))
;
; Explain why this interchange makes the program run slowly. Estimate how long it will take Louis's
; program to solve the eight-queens puzzle, assuming that the program in exercise 2.42 solves
; the puzzle in time T.

; Solution
; The modified queens procedure looks like
(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; Now we define some iteration-counting procedures for both procedure variants
(define (queens-iterations board-size)
  (let ([iterations 0])
    (define (queen-cols k)
      (if (= k 0)
          (list empty-board)
          (filter
            (lambda (positions) (safe? k positions))
            (flatmap
              (lambda (rest-of-queens)
                (map (lambda (new-row)
                       (set! iterations (+ iterations 1))
                       (adjoin-position new-row k rest-of-queens))
                     (enumerate-interval 1 board-size)))
              (queen-cols (- k 1))))))
    (queen-cols board-size)
    iterations))

(define (queens-slow-iterations board-size)
  (let ([iterations 0])
    (define (queen-cols k)
      (if (= k 0)
          (list empty-board)
          (filter
            (lambda (positions) (safe? k positions))
            (flatmap
              (lambda (new-row)
                (map (lambda (rest-of-queens)
                       (set! iterations (+ iterations 1))
                       (adjoin-position new-row k rest-of-queens))
                     (queen-cols (- k 1))))
              (enumerate-interval 1 board-size)))))
    (queen-cols board-size)
    iterations))

(module+ test
  (require rackunit)

  ; We see that the queens procedure from exercise 2.42 is significantly faster than queens-slow,
  ; even for 3x3 boards.
  (check <
    (* 3 (queens-iterations 3))
    (queens-slow-iterations 3))

  ; This difference in speed comes from the fact that (queen-cols (- k 1)) is evaluated multiple
  ; times at each step in the queens-slow procedure. Applicative-order evaluation allows us to spare
  ; all of this redundancy by placing the application of (queen-cols (- k 1)) outside of
  ; the outer lambda, thus applying it only once at each step (as in the original queens procedure).
  ;
  ; It is difficult to infer the time complexity for the queens and queens-slow procedures
  ; because of the filtration that happens at each step. We will ignore this filtration for now.
  ;
  ; Let k be the board size. We want to estimate the ceiling of the number applications
  ; of the inner lambda. We will use T(k) for the time complexity of the queens procedure
  ; and S(k) for queens-slow.
  ;
  ; T(k) can be easily estimated as the sum of the number of applications of the inner lambda at
  ; each step. This gives us
  ;   T(k) = sum_(i = 1)^k k^i ~ Theta(k^k),
  ; since, at each step, we apply the lambda k times for each application from the previous steps.
  ;
  ; For queens-slow, however, the complexity S = k^(k + 1), since we evaluate (queen-cols (- k 1))
  ; k times at each step.
  ;
  ; Asymptotically, S(k) ~ k T(k). Even if we apply the filtering procedure at each step,
  ; S(k) will still dominate T(k) asymptotically.

  )
