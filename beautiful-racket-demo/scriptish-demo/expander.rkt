#lang br/quicklang
(require racket/stxparam)
(provide (all-defined-out) (all-from-out br/quicklang))

(define-macro top #'begin)

(define-macro (assignment ID VAL) #'(define ID VAL))

(define (add/concat . xs)
  (cond
    [(andmap number? xs) (apply + xs)]
    [(ormap string? xs) (string-join (map ~a xs) "")]))
  
(define-macro-cases sumlike
  [(_ VAL) #'VAL]
  [(_ . VALS) #'(add/concat . VALS)])

(define-macro (object (K V) ...)
  #'(make-hash (list (cons K V) ...)))

(define-macro (func-def (ARG ...) STMT ...)
  (syntax/loc caller-stx
    (λ (ARG ...)
      (let/cc return-cc
        (syntax-parameterize ([return (make-rename-transformer #'return-cc)])
          STMT ... (void))))))

(define-syntax-parameter return
  (λ (stx) (error 'not-parameterized)))

(define-macro (dotted-id (BASE KEY ...))
  #'(for/fold ([val BASE])
              ([key (in-list (list 'KEY ...))])
      (cond
        [(hash-ref val key #f)]
        [(hash-ref val (symbol->string key) #f)]
        [else (error 'dotted-failure)])))

(define-macro func-app #'#%app)

(define-macro (if COND . STMTS)
  #'(when COND . STMTS))

(define-macro-cases comparison
  [(_ VAL) #'VAL]
  [(_ L == R) #'(equal? L R)]
  [(_ L != R) #'(not (equal? L R))])

(define-macro (while COND STMT ...)
  #'(let loop ()
      (when COND
        STMT ...
        (loop))))

(define alert displayln)

(define-macro (increment ID)
  #'(let ()
      (set! ID (add1 ID))
      ID))