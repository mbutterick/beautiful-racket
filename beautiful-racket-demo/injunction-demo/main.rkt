#lang br
(provide #%top-interaction #%module-begin
         (rename-out [my-datum #%datum]
                     [my-datum #%top]
                     [my-app #%app]))

(define-macro (my-datum . THING)
  (define datum (syntax->datum #'THING))
  (cond
    [(string? datum) #'"whee"]
    [(number? datum) #'42]
    [else #''kaboom]))

(define-macro (my-app FUNC . ARGS)
  #'(list FUNC . ARGS))

(module reader syntax/module-reader
  injunction-demo)