#lang racket/base
(require (for-syntax racket/base br/syntax) br/define)
(provide (all-defined-out))

(define-macro-cases report
  [(_ EXPR) #'(report EXPR EXPR)]
  [(_ EXPR NAME)
   #'(let ([expr-result EXPR])
       (eprintf "~a = ~v\n" 'NAME expr-result)
       expr-result)])

(define-macro-cases report-datum
  [(_ STX-EXPR) #`(report-datum STX-EXPR #,(syntax->datum #'STX-EXPR))]
  [(_ STX-EXPR NAME)
   #'(let ()
       (eprintf "~a = ~v\n" 'NAME (syntax->datum STX-EXPR))
       STX-EXPR)])

(define-macro (define-multi-version MULTI-NAME NAME)
  #'(define-macro (MULTI-NAME X (... ...))
      #'(begin (NAME X) (... ...))))

(define-multi-version report* report)
