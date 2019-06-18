#lang br/quicklang
(require "parser.rkt" brag/support)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-terms
  (:or "var" "=" ";" "+" "{" "}" "'" "\""
       ":" "," "(" ")" "//" "/*" "*/"
       "if" "while" "==" "!=" "function" "return" "++"))

(define scriptish-lexer
  (lexer-srcloc
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
    (let ()
    (token 'STRING (string-trim lexeme (substring lexeme 0 1))))]
   [whitespace (token 'WHITE #:skip? #t)]
   [any-char lexeme]))

(define (make-tokenizer ip [src #f])
  (port-count-lines! ip)
  (lexer-file-path src)
  (define (next-token) (scriptish-lexer ip))
  next-token)

(define (read-syntax src ip)
  (println src)
  (define parse-tree (parse src (make-tokenizer ip src)))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module scriptish-mod scriptish-demo/expander
         PT))))