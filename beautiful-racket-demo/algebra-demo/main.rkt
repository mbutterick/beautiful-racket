#lang br/quicklang
(require brag/support "grammar.rkt")
(provide top fun expr app)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-toks
  (:or "fun" "(" ")" "=" "+" ","))

(define tokenize-1
  (lexer
   [whitespace (token lexeme #:skip? #t)]
   [(from/stop-before "#" "\n") (token 'COMMENT #:skip? #t)]
   [reserved-toks lexeme]
   [(:+ alphabetic) (token 'ID (string->symbol lexeme))]
   [(:+ (char-set "0123456789")) (token 'INT (string->number lexeme))]))

(define-macro top #'#%module-begin)

(define-macro-cases fun
  [(_ VAR ARG0 EXPR) #'(define (VAR ARG0) EXPR)]
  [(_ VAR ARG0 ARG1 EXPR) #'(define (VAR ARG0 ARG1) EXPR)])

(define-macro-cases expr
  [(_ LEFT "+" RIGHT) #'(+ LEFT RIGHT)]
  [(_ OTHER) #'OTHER])

(define-macro app #'#%app)

(define (read-syntax src ip)
  (define parse-tree (parse src (λ () (tokenize-1 ip))))
  (strip-bindings
   (with-syntax ([PT parse-tree])
     #'(module algebra-mod algebra-demo
         PT))))