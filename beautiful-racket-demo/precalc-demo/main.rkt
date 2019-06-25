#lang br/quicklang
(require brag/support "grammar.rkt")
(provide top fun app add-or-sub mult-or-div)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-toks
  (:or "fun" "(" ")" "=" "+" "*" "/" "-" ","))

(define-lex-abbrev digits (char-set "0123456789"))

(define tokenize-1
  (lexer-srcloc
   [whitespace (token lexeme #:skip? #t)]
   [(:or (from/stop-before "#" "\n")
         (from/to "/*" "*/")) (token 'COMMENT #:skip? #t)]
   [reserved-toks lexeme]
   [(:seq (:? "-") (:+ (:or alphabetic) digits))
    (let ([maybe-num (string->number lexeme)])
      (if maybe-num
          (token 'INT maybe-num)
          (token 'ID (string->symbol lexeme))))]))

(define-macro top #'#%module-begin)

(define-macro (fun VAR (ARGVAR ...) EXPR)
  #'(define (VAR ARGVAR ...) EXPR))

(define-macro-cases add-or-sub
  [(_ LEFT "+" RIGHT) #'(+ LEFT RIGHT)]
  [(_ LEFT "-" RIGHT) #'(- LEFT RIGHT)]
  [(_ OTHER) #'OTHER])

(define-macro-cases mult-or-div
  [(_ LEFT "*" RIGHT) #'(* LEFT RIGHT)]
  [(_ LEFT "/" RIGHT) #'(/ LEFT RIGHT)]
  [(_ OTHER) #'OTHER])

(define-macro app #'#%app)

(define (read-syntax src ip)
  (port-count-lines! ip)
  (lexer-file-path src)
  (define parse-tree (parse src (λ () (tokenize-1 ip))))
  (strip-bindings
   (with-syntax ([PT parse-tree])
     #'(module precalc-mod precalc-demo
         PT))))