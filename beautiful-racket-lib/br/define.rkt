#lang racket/base
(require (for-syntax racket/base syntax/parse) br/macro)
(provide (all-defined-out) (all-from-out br/macro))

(define-syntax (define-cases stx)
  (syntax-parse stx
    #:literals (syntax)
    [(_ id:id)
     (raise-syntax-error 'define-cases "no cases given" (syntax->datum #'id))]
    [(_ id:id [(_ . pat-args:expr) . body:expr] ...)
     #'(define id
         (case-lambda
           [pat-args . body] ...
           [rest-pat (apply raise-arity-error 'id (normalize-arity (map length '(pat-args ...))) rest-pat)]))]
    [else (raise-syntax-error
           'define-cases
           "no matching case for calling pattern"
           (syntax->datum stx))]))
