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
  (for/list ([tok (in-port lex ip)])
    tok))

(define (parse src toks)
  (define heptatoks
    (let loop ([toks toks][acc null])
      (if (empty? toks)
          (reverse acc)
          (loop (drop toks 7) (cons (take toks 7) acc)))))
  (for/list ([heptatok (in-list heptatoks)])
    (integer->char
     (for/sum ([val (in-list heptatok)]
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

(define-macro (my-module-begin PT)
  #'(#%module-begin
     (display (list->string 'PT))))
(provide (rename-out [my-module-begin #%module-begin]))