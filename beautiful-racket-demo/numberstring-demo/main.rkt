#lang br
(require racket/sequence)

(module reader br
  (provide read-syntax)
  (define (read-syntax path ip)
    (strip-context
     #`(module mod numberstring-demo
         #,@(map string->number (regexp-match* #rx"." (string-trim (port->string ip))))))))

(define (ones->word num)
  (case num
    [(1) "one"][(2) "two"][(3) "three"][(4) "four"][(5) "five"]
    [(6) "six"][(7) "seven"][(8) "eight"][(9) "nine"]))

(define (tens->word num)
  (case num
    [(2) "twenty"][(3) "thirty"][(4) "forty"][(5) "fifty"]
    [(6) "sixty"][(7) "seventy"][(8) "eighty"][(9) "ninety"]
    [else (number->string num)]))

(define (two-digit->word num)
  (case num
    [(10) "ten"][(11) "eleven"][(12) "twelve"][(13) "thirteen"][(14) "fourteen"]
    [(15) "fifteen"][(16) "sixteen"][(17) "seventeen"][(18) "eighteen"][(19) "nineteen"]
    [else (string-join (cons (tens->word (quotient num 10))
                             (if (positive? (modulo num 10))
                                 (list (ones->word (modulo num 10)))
                                 null)) "-")]))

(define (triple->string triple)
  (match-define (list h t o) triple)
  (string-join
   (append
    (if (positive? h)
        (list (ones->word h) "hundred")
        null)
    (if (positive? t)
        (list (two-digit->word (+ (* 10 t) o)))
        (list (ones->word o)))) " "))

(define (ones triple) (format "~a" (triple->string triple)))
(define (thousands triple) (format "~a thousand" (triple->string triple)))
(define (millions triple) (format "~a million" (triple->string triple)))

(provide #%datum #%top-interaction (rename-out [mb #%module-begin]))
(define-macro (mb . DIGITS)
  #'(#%module-begin
     (define digits (list . DIGITS))
     (define padded-digits (append (make-list (- 9 (length digits)) 0) digits))
     (display (string-join (reverse (for/list ([triple (in-slice 3 (reverse padded-digits))]
                                               [quantifier (list ones thousands millions)]
                                               #:unless (equal? triple '(0 0 0)))
                                      (quantifier (reverse triple)))) ", "))))