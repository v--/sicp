#lang sicp

; Exercise 2.55
;
; Eva Lu Ator types to the interpreter the expression
;   (car ''abracadabra)
; To her surprise, the interpreter prints back quote. Explain.

; Solution
; Since the quotation syntax is simply syntax sugar for the application of the quote primitive,
; ''abracadabra can be expanded as follows:

(module+ test
  (require rackunit)

  (define-simple-check (check-abracadabra? value)
    (equal? value ''abracadabra))

  (check-abracadabra? (quote 'abracadabra))
  (check-abracadabra? (quote (quote abracadabra)))
  (check-abracadabra? (list (quote quote) (quote abracadabra)))
  (check-abracadabra? (list 'quote 'abracadabra))
  (check-abracadabra? '(quote abracadabra))

  ; The first element of the '(quote abracadabra) list is, of course, 'quote.

  )
