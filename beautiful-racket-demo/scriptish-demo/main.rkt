#lang br/quicklang
(require "grammar.rkt" brag/support)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-terms
  (:or "var" "=" ";" "+" "{" "}" "'" "\""
       ":" "," "(" ")" "//" "/*" "*/"
       "if" "else" "while" "==" "!=" "function" "return" "++"))

(define tokenize-1
  (lexer-srcloc
   [(:or (from/stop-before "//" "\n")
         (from/to "/*" "*/")) (token 'COMMENT #:skip? #t)]
   [reserved-terms lexeme]
   [(:+ (:- (:or alphabetic punctuation) "." reserved-terms))
    (token 'ID (string->symbol lexeme))]
   [(:+ (:- (:or alphabetic punctuation) reserved-terms))
    (token 'DEREF (map string->symbol (string-split lexeme ".")))]
   [(:+ (char-set "0123456789"))
    (token 'INTEGER (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING (string-trim lexeme (substring lexeme 0 1)))]
   [whitespace (token 'WHITE #:skip? #t)]
   [any-char lexeme]))

(define (read-syntax src ip)
  (port-count-lines! ip)
  (lexer-file-path ip)
  (define parse-tree (parse src (λ () (tokenize-1 ip))))
  (strip-bindings
   (with-syntax ([PT parse-tree])
     #'(module scriptish-mod scriptish-demo/expander
         PT))))