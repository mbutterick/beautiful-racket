#lang br
(require racket/file)

#|
(define src (file->string "source.txt"))

(define strs (string-split src))

(define toks (map (λ (str) (or (string->number str) (string->symbol str))) strs))

(define expr (list (second toks) (first toks) (third toks)))

(eval expr (make-base-namespace))
|#

#|
(eval
 (match (for/list ([str (in-list (string-split (file->string "source.txt")))])
                  (or (string->number str) (string->symbol str)))
   [(list num1 op num2) (list op num1 num2)])
 (make-base-namespace))
|#

(define (eval-src src)
  (eval
   (match (for/list ([str (in-list (string-split src))])
                    (or (string->number str) (string->symbol str)))
     [(list num1 op num2) (list op num1 num2)]) (make-base-namespace)))


(module reader br
  (provide read-syntax)
  (define (read-syntax name ip)
    `(module mod "main.rkt"
       ,(port->string ip))))

(provide #%datum #%top-interaction (rename-out [mb #%module-begin]))
(define-macro (mb SRC)
  #'(#%module-begin
     (eval-src SRC)))