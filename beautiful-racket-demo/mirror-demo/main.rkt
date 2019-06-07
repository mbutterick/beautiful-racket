#lang br/quicklang

(module reader br
  (provide (rename-out [rs read-syntax]))
  (define (rs src ip)
    (define toks (for/list ([tok (in-port (λ (p) (read-syntax src ip)) ip)])
                           tok))
    (strip-context
     (with-syntax ([(PT ...) toks])
       #'(module _ mirror-demo
           PT ...)))))

(provide (except-out (all-from-out br/quicklang) #%module-begin)
         (rename-out [mb #%module-begin]))

(define-macro (mb PT ...)
  #'(#%module-begin
     PT ...))