#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (for/list ([tok (in-port read-char ip)])
    tok))

(define (parse src toks)
  (for/list ([tok (in-list toks)])
    (define int (char->integer tok))
    (for/list ([bit (in-range 7)])
      (if (bitwise-bit-set? int bit)
          'taco
          null))))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse src toks))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module tacofied br
         (for-each displayln 'PT)))))