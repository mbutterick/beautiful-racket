#lang br
(require racket/stxparam)
(provide (all-defined-out)
         #%top-interaction #%top #%app
         (rename-out [my-datum #%datum]))

(define-macro top #'#%module-begin)

(define-macro-cases ternary
  [(_ EXPR) #'EXPR]
  [(_ COND TRUE-EXPR FALSE-EXPR) #'(if COND TRUE-EXPR FALSE-EXPR)])

(define-macro-cases logical-or
  [(_ VAL) #'VAL]
  [(_ L "||" R) #'(or L R)])

(define-macro-cases logical-and
  [(_ VAL) #'VAL]
  [(_ L "&&" R) #'(and L R)])

(define-macro (my-datum . VAL)
  (with-syntax ([NEW-VAL (let ([val (syntax->datum #'VAL)])
                           (cond
                             [(and (integer? val) (inexact? val))
                              (inexact->exact val)]
                             [(boolean? val) (if val "True" "False")]
                             [else val]))])
    #'(#%datum . NEW-VAL)))

(define-macro (var ID VAL) #'(define ID VAL))

(define (add/concat . xs)
  (cond
    [(andmap number? xs) (apply + xs)]
    [(ormap string? xs) (string-join (map ~a xs) "")]))
  
(define-macro-cases add-or-sub
  [(_ LEFT "+" RIGHT) #'(add/concat LEFT RIGHT)]
  [(_ LEFT "-" RIGHT) #'(- LEFT RIGHT)]
  [(_ OTHER) #'OTHER])

(define-macro-cases mult-or-div
  [(_ LEFT "*" RIGHT) #'(* LEFT RIGHT)]
  [(_ LEFT "/" RIGHT) #'(/ LEFT RIGHT)]
  [(_ OTHER) #'OTHER])

(define-macro (object (K V) ...)
  #'(make-hash (list (cons K V) ...)))

(define-syntax-parameter return
  (λ (stx) (error 'not-parameterized)))

(define-macro (fun (ARG ...) STMT ...)
  (syntax/loc caller-stx
    (λ (ARG ...)
      (let/cc return-cc
        (syntax-parameterize ([return (make-rename-transformer #'return-cc)])
          (void) STMT ...)))))

(define-macro (defun ID (ARG ...) STMT ...)
  #'(define ID (fun (ARG ...) STMT ...)))

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

(define-macro-cases if-else
  [(_ COND TSTMT ... "else" FSTMT ...) #'(cond
                                           [COND TSTMT ...]
                                           [else FSTMT ...])]
  [(_ COND STMT ...) #'(when COND STMT ...)])

(define-macro-cases equal-or-not
  [(_ VAL) #'VAL]
  [(_ L "==" R) #'(equal? L R)]
  [(_ L "!=" R) #'(not (equal? L R))])

(define-macro-cases gt-or-lt
  [(_ VAL) #'VAL]
  [(_ L "<" R) #'(< L R)]
  [(_ L "<=" R) #'(<= L R)]
  [(_ L ">" R) #'(> L R)]
  [(_ L ">=" R) #'(>= L R)])

(define-macro (while COND STMT ...)
  #'(let loop ()
      (when COND
        STMT ...
        (loop))))

(define (alert x) (displayln (format "ALERT! ~a" x)))

#;(require racket/gui)
#;(define (alert text)
    (define dialog (instantiate dialog% ("Alert")))
    (new message% [parent dialog] [label text])
    (define panel (new horizontal-panel% [parent dialog]
                       [alignment '(center center)]))
    (new button% [parent panel] [label "Ok"]
         [callback (lambda (button event)
                     (send dialog show #f))])
    (send dialog show #t))

(define-macro-cases increment
  [(_ ID) #'ID]
  [(_ "++" ID) #'(let ()
                   (set! ID (add1 ID))
                   ID)]
  [(_ "--" ID) #'(let ()
                   (set! ID (sub1 ID))
                   ID)]
  [(_ ID "++") #'(begin0
                   ID
                   (set! ID (add1 ID)))]
  [(_ ID "--") #'(begin0
                   ID
                   (set! ID (sub1 ID)))])


(define-macro-cases reassignment
  [(_ ID) #'ID]
  [(_ ID "+=" EXPR) #'(let ()
                        (set! ID (+ ID EXPR))
                        ID)]
  [(_ ID "-=" EXPR) #'(let ()
                        (set! ID (- ID EXPR))
                        ID)])