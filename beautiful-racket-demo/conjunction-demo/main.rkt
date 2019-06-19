#lang br/quicklang

(module reader br
  (provide read-syntax)
  (define (read-syntax name port)
    (define s-exprs (let loop ([toks null])
                      (define tok (read port))
                      (if (eof-object? tok)
                          (reverse toks)
                          (loop (cons tok toks)))))
    (strip-bindings
     (with-syntax ([(EXPR ...) s-exprs])
       #'(module read-only-mod conjunction-demo
           EXPR ...)))))

(define (convert-expr x)
  (cond
    [(list? x) (map convert-expr x)]
    [(number? x) 42]
    [(string? x) "whee"]
    [else 'kaboom]))

(define-macro (my-module-begin EXPR ...)
  #'(#%module-begin
     (convert-expr 'EXPR) ...))
(provide (rename-out [my-module-begin #%module-begin]))
