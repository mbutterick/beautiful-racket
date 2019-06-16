#lang br

(define (convert-expr x)
  (let loop ([x x])
    (cond
      [(list? x) (map loop x)]
      [(number? x) 42]
      [(string? x) "whee"]
      [else 'kaboom])))

(define-macro (dsl-module-begin EXPR ...)
  #'(#%module-begin
     (convert-expr 'EXPR) ...))
(provide (rename-out [dsl-module-begin #%module-begin]))
