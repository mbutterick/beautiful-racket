#lang br/quicklang
(require "grammar.rkt" brag/support)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-terms
  (:or "var" "=" ";" "+" "{" "}" "'" "\""
       ":" "," "(" ")" "//" "/*" "*/"
       "if" "while" "==" "!=" "function" "return" "++"))

(define tokenize
  (lexer
   [(:or (from/stop-before "//" "\n")
         (from/to "/*" "*/")) (token 'COMMENT #:skip? #t)]
   [reserved-terms (token lexeme (string->symbol lexeme))]
   [(:+ (:- (:or alphabetic punctuation) "." reserved-terms))
    (token 'ID (string->symbol lexeme))]
   [(:+ (:- (:or alphabetic punctuation) reserved-terms))
    (token 'DOTTED-ID (map string->symbol (string-split lexeme ".")))]
   [(:+ (char-set "0123456789"))
    (token 'INTEGER (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING (string-trim lexeme (substring lexeme 0 1)))]
   [whitespace (token 'WHITE #:skip? #t)]
   [any-char lexeme]))

(define (read-syntax src ip)
  (define parse-tree (parse (λ () (tokenize ip))))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module you-win scriptish-demo/expander
         PT))))