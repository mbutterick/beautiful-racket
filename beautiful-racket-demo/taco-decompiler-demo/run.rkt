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
  (define src-string (list->string parse-tree))
  (define racket-toks
    (for/list ([tok (in-port read (open-input-string src-string))])
              tok))
  (strip-context
   (with-syntax ([RACKET-TOKS racket-toks])
     #'(module untaco racket
         . RACKET-TOKS))))