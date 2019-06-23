#lang br/quicklang
(require brag/support "grammar.rkt")
(provide taco-program taco-leaf 
         taco not-a-taco
         show
         #%module-begin)

(module+ reader
  (provide read-syntax))

(define (tokenize-1 ip)
  (define lex
    (lexer
     ["#$" lexeme]
     ["%" lexeme]
     [any-char (lex input-port)]))
  (lex ip))

(define (taco-program . pieces) pieces)

(define (taco-leaf . pieces)
  (integer->char
   (for/sum ([taco-or-not (in-list pieces)]
             [pow (in-naturals)])
     (* taco-or-not (expt 2 pow)))))

(define (taco) 1)

(define (not-a-taco) 0)

(define (show pt)
  (display (apply string pt)))

(define (read-syntax src ip)
  (define token-thunk (λ () (tokenize-1 ip)))
  (define parse-tree (parse src token-thunk))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module winner taco-victory-demo
         (show PT)))))