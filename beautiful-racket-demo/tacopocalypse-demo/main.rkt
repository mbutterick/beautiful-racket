#lang br/quicklang
(require brag/support racket/sequence)

(module+ reader
  (provide read-syntax))

(define lex
  (lexer
   ["#$" null]
   ["%" 'taco]
   [any-char (lex input-port)]))

(define (tokenize ip)
  (define toklets
    (for/list ([toklet (in-port lex ip)])
      toklet))
  (for/list ([tok (in-slice 7 toklets)])
    tok))

(define (parse src toks)
  (for/list ([tok (in-list toks)])
    (integer->char
     (for/sum ([val (in-list tok)]
               [power (in-naturals)]
               #:when (eq? val 'taco))
       (expt 2 power)))))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse src toks))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module untaco racket
         (display (list->string 'PT))))))