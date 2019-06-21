#lang br/quicklang
(require brag/support)

(module+ reader
  (provide read-syntax))

(define lex
  (lexer
   ["#$" null]
   ["%" 'taco]
   [any-char (lex input-port)]))

(define (tokenize ip)
  (define toklets (for/list ([toklet (in-port lex ip)])
                    toklet))
  (let loop ([toklets toklets][acc null])
    (if (null? toklets)
        (reverse acc)
        (loop (drop toklets 7) (cons (take toklets 7) acc)))))

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
     #'(module taco-mod tacopocalypse-demo
         PT))))

(define-macro (mb PT)
  #'(#%module-begin
     (display (list->string 'PT))))
(provide (rename-out [mb #%module-begin]))