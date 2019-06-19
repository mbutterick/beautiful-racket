#lang br

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
