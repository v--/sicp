#lang sicp

; Exercise 2.22
;
; Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it evolves an iterative process:
;
; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons (square (car things))
;                     answer))))
;   (iter items null))
;
; Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?
;
; Louis then tries to fix his bug by interchanging the arguments to cons:
;
; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons answer
;                     (square (car things))))))
;   (iter items null))
;
; This doesn't work either. Explain.

; Solution

(module+ test
  (require rackunit)

  ; These will be used for the tests
  (define items (list 1 2 3))
  (define squared (list 1 4 9))

  ; Because there are only forward references in the list's representation in terms of pairs,
  ; appending a new item to a list requires either mutating the cdr of the last (non-null) pair
  ; or constructing a new list.
  ;
  ; When constructing a new list using a recursive procedure, such as the one given in exercise 2.21,

  (define (square-list items)
    (if (null? items)
        null
        (cons (square (car items)) (square-list (cdr items)))))

  ; , the list is actually constructed right-to-left and the recursion stack "remembers" where to add
  ; the new pair. An iterative procedure could be implemented using the append primitive, which
  ; constructs a new list on each step:

  (define (square-list-iter items)
    (define (iter things answer)
      (if (null? things)
          answer
          (iter (cdr things)
                (append answer
                        (list (square (car things)))))))
    (iter items null))

  (check-equal? (square-list-iter items) squared)

  ; To inspect what is wrong with the two given procedures, we'll use substitution on
  ; the nested iter procedures to expand the recursion and see where the computation goes wrong.
  ;
  ; In the first version of square-list, the result is obtained by starting from the last element
  ; and appending the previous value at each step, thus giving the reverse list.

  (define reversed (list 9 4 1))

  (define-simple-check (check-reversed? lst)
    (equal? lst reversed))

  (define (iter-1 things answer)
    (if (null? things)
        answer
        (iter-1 (cdr things)
                (cons (square (car things))
                      answer))))

  (check-reversed? (iter-1 (list 1 2 3) null))
  (check-reversed? (iter-1 (list 2 3) (list 1)))
  (check-reversed? (iter-1 (list 3) (list 4 1)))
  (check-reversed? (iter-1 (list) (list 9 4 1)))

  ; In the second version of square-list, the result is not a list because we attempt to store
  ; values in the cdr and references in the car. It would have been a perfectly valid implementation
  ; if the lists used backward references instead of forward references.

  (define strange (cons (cons (cons null 1) 4) 9))

  (define-simple-check (check-strange? lst)
    (equal? lst strange))

  (define (iter-2 things answer)
    (if (null? things)
        answer
        (iter-2 (cdr things)
                (cons answer
                      (square (car things))))))

  (check-strange? (iter-2 (list 1 2 3) (list)))
  (check-strange? (iter-2 (list 2 3) (cons null 1)))
  (check-strange? (iter-2 (list 3) (cons (cons null 1) 4)))
  (check-strange? (iter-2 (list) (cons (cons (cons null 1) 4) 9))))
