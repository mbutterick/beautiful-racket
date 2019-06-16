#lang br
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
       #'(module read-only-mod racket
           EXPR ...)))))