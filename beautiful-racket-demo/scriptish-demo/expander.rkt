#lang br/quicklang
(require racket/stxparam)
(provide (all-defined-out))

(define-macro top #'#%module-begin)

(define-macro (var ID VAL) #'(define ID VAL))

(define (add/concat . xs)
  (cond
    [(andmap number? xs) (apply + xs)]
    [(ormap string? xs) (string-join (map ~a xs) "")]))
  
(define-macro-cases add-or-sub
  [(_ VAL) #'VAL]
  [(_ . VALS) #'(add/concat . VALS)])

(define-macro (object (K V) ...)
  #'(make-hash (list (cons K V) ...)))

(define-syntax-parameter return
  (λ (stx) (error 'not-parameterized)))

(define-macro (fun (ARG ...) STMT ...)
  (syntax/loc caller-stx
    (λ (ARG ...)
      (let/cc return-cc
        (syntax-parameterize ([return (make-rename-transformer #'return-cc)])
          STMT ... (void))))))

(define (resolve-deref base . keys)
  (for/fold ([val base])
            ([key (in-list keys)])
    (cond
      [(and
        (hash? val)
        (cond
          [(hash-ref val key #f)]
          [(hash-ref val (symbol->string key) #f)]
          [else #f]))]
      [else (error 'deref-failure)])))

(define-macro (deref (BASE KEY ...))
  #'(resolve-deref BASE 'KEY ...))

(define-macro app #'#%app)

(define-macro-cases if
  [(_ COND TSTMT ... "else" FSTMT ...) #'(cond
                                           [COND TSTMT ...]
                                           [else FSTMT ...])]
  [(_ COND STMT ...) #'(when COND STMT ...)])

(define-macro-cases comparison
  [(_ VAL) #'VAL]
  [(_ L "==" R) #'(equal? L R)]
  [(_ L "!=" R) #'(not (equal? L R))])

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