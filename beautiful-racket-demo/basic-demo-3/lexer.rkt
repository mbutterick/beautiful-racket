#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define-lex-abbrev reserved-terms (:or "print" "goto" "end" "+" ":" ";" "let" "=" "input" "-" "*" "/" "^" "mod" "(" ")" "if" "then" "else" "<" ">" "<>" "and" "or" "not" "gosub" "return" "for" "to" "step" "next" "def" "," "import" "export"))

(define-lex-abbrev racket-id-kapu
  (:or whitespace (char-set "()[]{}\",'`;#|\\")))

(define basic-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/stop-before "rem" "\n") (token 'REM lexeme)]
   [(:seq "[" (:+ (:~ racket-id-kapu)) "]")
    (token 'RACKET-ID
           (string->symbol (trim-ends "[" lexeme "]")))] 
   [reserved-terms (token lexeme lexeme)]
   [(:seq alphabetic (:* (:or alphabetic numeric "$")))
    (token 'ID (string->symbol lexeme))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:or (:seq (:? digits) "." digits)
         (:seq digits "."))
    (token 'DECIMAL (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]))

(provide basic-lexer)
