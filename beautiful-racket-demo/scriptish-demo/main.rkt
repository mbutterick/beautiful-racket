#lang br/quicklang
(require "grammar.rkt" brag/support)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-toks
  (:or "var" "=" ";" "{" "}" "//" "/*" "*/"
       "+" "*" "/" "-"
       "'" "\""
       ":" "," "(" ")" 
       "if" "else" "while" "?"
       "==" "!=" "<=" "<" ">=" ">" "&&" "||"
       "function"
       "return" "++" "--" "+=" "-="))

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define tokenize-1
  (lexer-srcloc
   [(:or (from/stop-before "//" "\n")
         (from/to "/*" "*/")) (token 'COMMENT #:skip? #t)]
   [reserved-toks lexeme]
   [(:seq (:? "-") (:or (:seq (:? digits) "." digits)
                        (:seq digits (:? "."))))
    (token 'NUMBER (string->number lexeme))]
   [(:seq (:+ (:- (:or alphabetic punctuation digits) reserved-toks)))
    (if (string-contains? lexeme ".")
        (token 'DEREF (map string->symbol (string-split lexeme ".")))
        (token 'ID (string->symbol lexeme)))]
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