#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (for/list ([tok (in-port read ip)])
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
     #'(module untaco taco-decompiler-demo
         PT))))

(define-macro (mb PT)
  #'(#%module-begin
     (display (list->string 'PT))))
(provide (rename-out [mb #%module-begin]))