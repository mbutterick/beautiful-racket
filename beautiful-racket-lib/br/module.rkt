#lang at-exp racket/base
(require (for-syntax racket/base racket/match))

;; adapted from the far superior work of Jay McCarthy
;; https://github.com/jeapostrophe/remix/blob/master/remix/stx0.rkt

(provide module/lang module*/lang)

(define-syntax (module/lang stx)
  (do-lang 'module/lang #'module stx))

(define-syntax (module*/lang stx)
  (do-lang 'module*/lang #'module* stx))

(define-for-syntax (do-lang caller-id module-maker-stx stx)
  (syntax-case stx ()
    [(_ MODULE-NAME . STX-STRS)
     (identifier? #'MODULE-NAME)
     (let ()
       (define input-port (syntax-strings->input-port
                           (syntax-source stx)
                           (syntax->list #'STX-STRS)))
       (define module-body (parameterize ([read-accept-reader #t]
                                          [read-accept-lang #t])
                             (read-syntax #'MODULE-NAME input-port)))
       (with-handlers ([exn:fail:syntax?
                        (λ (exn) (raise-syntax-error caller-id "body did not read as module" stx module-body))])
         (with-syntax ([(_ _ MODULE-LANG . REST) module-body]
                       [MODULE-MAKER module-maker-stx])
           (syntax/loc stx (MODULE-MAKER MODULE-NAME MODULE-LANG . REST)))))]))


(define-for-syntax (syntax-strings->input-port name first-ss)
  #;(-> any/c (listof syntax?) input-port?)
  (unless (and (pair? first-ss) (andmap syntax? first-ss))
    (raise-argument-error 'syntax-strings->input-port "list of syntax" first-ss))
  
  (define line 1)
  (define col 0)
  (define pos 1)
  (define current-idx #f)
  (define current-bs #f)
  (define next-ss first-ss)

  (define (consume-ss!)
    (match next-ss
      ['() (void)]
      [(cons ss more-ss)     
       (set! line (syntax-line ss))
       (set! col (syntax-column ss))
       (set! pos (syntax-position ss))
       (set! current-bs (string->bytes/utf-8 (syntax->datum ss)))
       (set! current-idx 0)
       (set! next-ss more-ss)]))

  (consume-ss!)

  (define (read-in bs)
    (cond
      [(not current-bs)
       (match next-ss
         ['() eof]
         [(cons ss more-ss)
          (consume-ss!)
          (read-in bs)])]
      [(< current-idx (bytes-length current-bs))
       (define how-many
         (min (bytes-length bs)
              (- (bytes-length current-bs)
                 current-idx)))
       (define end (+ current-idx how-many))
       (bytes-copy! bs 0 current-bs current-idx end)
       (set! current-idx end)
       (set! col (+ (or col 0) how-many))
       (set! pos (+ (or pos 0) how-many))
       (unless (< current-idx (bytes-length current-bs))
         (consume-ss!))
       how-many]
      [else
       (set! current-bs #f)
       (read-in bs)]))
  
  (define (get-location) (values line col pos))

  (parameterize ([port-count-lines-enabled #t])
    (make-input-port name read-in #f void #f #f
                     get-location void #f #f)))


;; can't run tests without the underlying languages installed
#;(module+ test
  (require rackunit)

  @module/lang[pollen-pre]{
 #lang pollen
 ◊whoomp{there it is}
}

  (require 'pollen-pre)
  (check-equal? doc "'(whoomp \"there it is\")")

  #|
;; raises "body did not read as module" error
  @module/lang[pollen-pre]{
 ◊whoomp{there it is}
}
  |#
  
  @module/lang[brag]{
 #lang brag
 top: /"x"
}

  (require 'brag)
  (check-equal? (parse-to-datum "x") '(top))

  )
