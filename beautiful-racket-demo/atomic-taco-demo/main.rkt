#lang br
(provide #%top-interaction #%module-begin
         (rename-out [my-datum #%datum]
                     [my-datum #%top]
                     [my-app #%app]))

(define-syntax (my-datum stx)
  (syntax-case stx ()
    [(_ . THING) #''taco]))

(define-syntax (my-app stx)
  (syntax-case stx ()
    [(_ FUNC . ARGS) #'(list (my-datum) . ARGS)]))

(module reader syntax/module-reader
  atomic-taco-demo)