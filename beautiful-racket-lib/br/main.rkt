#lang racket/base
(require racket/provide racket/list racket/string racket/format racket/match racket/port racket/function racket/provide
         br/define br/syntax br/datum br/debug br/cond br/list br/reader-utils br/module racket/class racket/vector racket/promise
         (for-syntax racket/base racket/syntax br/syntax br/debug br/define br/datum))
(provide (all-from-out racket/base)
         (all-from-out racket/list racket/string racket/format racket/match racket/port racket/function racket/provide
                       br/syntax br/datum br/debug br/cond br/list br/reader-utils br/module racket/class racket/vector racket/promise br/define)
         (for-syntax (all-from-out racket/base racket/syntax br/syntax br/debug br/datum))
         (for-syntax caller-stx with-shared-id)) ; from br/define
         
(provide evaluate)
(define-macro (evaluate DATUM)
  #'(begin
      (define-namespace-anchor nsa)
      (eval DATUM (namespace-anchor->namespace nsa))))

(provide really-dynamic-require)
(define-macro (really-dynamic-require . ARGS)
  #'(parameterize ([current-namespace (make-base-namespace)])
      (dynamic-require . ARGS)))

(module reader racket/base
  (provide (rename-out
            [br-read read]
            [br-read-syntax read-syntax]
            [at:get-info get-info]))

  (module at-reader syntax/module-reader
    #:language 'br
    #:info br-get-info
    #:read my-read
    #:read-syntax my-read-syntax
    (require br/get-info (prefix-in at: scribble/reader))
    (define (my-read ip) (syntax->datum (my-read-syntax ip)))
    (define (my-read-syntax src ip)
      (parameterize ([current-readtable (at:make-at-readtable #:command-char #\◊)])
        (at:read-syntax src ip))))

  (require debug/reader (prefix-in at: 'at-reader))

  #|
Use wrap-reader on the whole-module read function that would be exported
by the reader module, not the single-expression read function like
at:read-syntax that you deal with within syntax/module-reader or normal use.
|#

  (define br-read (wrap-reader at:read))
  (define br-read-syntax (wrap-reader at:read-syntax)))