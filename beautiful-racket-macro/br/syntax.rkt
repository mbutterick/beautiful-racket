#lang racket/base
(require (for-syntax
          racket/base
          "private/generate-literals.rkt")
         racket/list
         racket/match
         racket/syntax
         racket/format
         syntax/stx
         syntax/strip-context
         syntax/parse
         "macro.rkt"
         "private/syntax-flatten.rkt")
(provide (all-defined-out)
         syntax-flatten
         stx-map
         strip-context ;; under both names
         (rename-out [strip-context strip-bindings]
                     [replace-context replace-bindings]
                     [stx-map syntax-map]
                     [syntax-flatten stx-flatten]
                     [prefix-id prefix-ids]
                     [suffix-id suffix-ids]
                     [infix-id infix-ids]))

(module+ test (require rackunit))


(define-macro-cases syntax-parse/easy
  [(_ STX [PAT . BODY] ... [else . ELSEBODY])
   (with-syntax ([(PAT ...) (map (λ (stx) (literalize-pat stx #'~literal)) (syntax->list #'(PAT ...)))])
     #'(syntax-parse (syntax-case STX () [any #'any])
         [PAT . BODY] ... [else . ELSEBODY]))])


(define-macro-cases pattern-case
  [(_ EXPR ... [else . ELSEBODY]) #'(syntax-parse/easy EXPR ... 
                                                       [else . ELSEBODY])]
  [(_ STX-ARG PAT+BODY ...)
   #'(pattern-case STX-ARG
                   PAT+BODY ...
                   [else (raise-syntax-error 'pattern-case
                                             (format "unable to match pattern for ~v" (syntax->datum STX-ARG)))])])

(module+ test
  (define (pc stx)
    (pattern-case stx
                  [(a 2ND c) #'2ND]
                  [(LEFT RIGHT) #'LEFT]
                  [(g) #'hooray]))
  (check-equal? (syntax->datum (pc #'(a b c))) 'b)
  (check-equal? (syntax->datum (pc #'(x y))) 'x)
  (check-equal? (syntax->datum (pc #'(g))) 'hooray)
  (check-exn exn:fail:syntax? (λ () (pc #'(f)))))


(define-macro (pattern-case-filter STX-ARG PAT+BODY ...)
  #'(let* ([arg STX-ARG]
           [stxs (or (and (syntax? arg) (syntax->list arg)) arg)])
      (unless (and (list? stxs) (andmap syntax? stxs))
        (raise-syntax-error 'pattern-case-filter
                            (format "~v cannot be made into a list of syntax objects" (syntax->datum arg))))
      (for*/list ([stx (in-list stxs)]
                  [result (in-value (pattern-case stx PAT+BODY ... [else #f]))]
                  #:when result)
        result)))


(module+ test
  (define (pcf x)
    (pattern-case-filter x
                         [(1ST 2ND 3RD) #'2ND]
                         [(LEFT RIGHT) #'LEFT]))
  (check-equal? (map syntax-e (pcf #'((a b c) (x y) (f)))) '(b x)))


(define-macro-cases with-pattern
  [(_ () . BODY) #'(begin . BODY)]
  [(_ ([PAT0 STX0] PAT+STX ...) . BODY)
   #'(syntax-parse/easy STX0 
                        [PAT0 (with-pattern (PAT+STX ...) (let () (void) . BODY))]
                        [else (raise-syntax-error 'with-pattern
                                                  (format "unable to match pattern ~a" 'PAT0) STX0)])])

(module+ test
  (check-equal? (syntax->datum (with-pattern ([foo 'foo])
                                 #'zzz)) 'zzz)
  (check-exn exn:fail:syntax? (λ () (with-pattern ([42 'foo])
                                      #'zzz)))
  (check-exn exn:fail:syntax? (λ () (with-pattern ([bar 'foo])
                                      #'zzz)))
  (check-equal? (syntax->datum (with-pattern ([(foo BAR) '(foo 42)])
                                 #'BAR)) 42)
  (check-exn exn:fail:syntax? (λ () (with-pattern ([(foo BAR) '(zam 42)])
                                      #'BAR))))


(define-macro (format-string FMT ID0 ID ...)
  #'(datum->syntax ID0 (format FMT (syntax->datum ID0) (syntax->datum ID) ...)))

(define (->unsyntax x) (if (syntax? x) (syntax->datum x) x))

(define (stx-join stxs)
  (apply string-append (map (compose1 ~a ->unsyntax) stxs)))

(define (*fix-base loc-arg ctx-arg prefixes base-or-bases suffixes)
  (define list-mode? (or (list? base-or-bases) (syntax->list base-or-bases)))
  (define bases (if list-mode?
                    (or (syntax->list base-or-bases) base-or-bases)
                    (list base-or-bases)))
  (define result (map (λ (base) (format-id (or ctx-arg base) "~a~a~a" (stx-join prefixes) (syntax-e base) (stx-join suffixes)
                                           #:source loc-arg)) bases))
  (if list-mode? result (car result)))


(define (prefix-id #:source [loc-arg #f] #:context [ctx-arg #f] . args)
  ((match-lambda
     [(list prefixes ... base-or-bases)
      (*fix-base loc-arg ctx-arg prefixes base-or-bases empty)]) args))

(define (infix-id #:source [loc-arg #f] #:context [ctx-arg #f] . args)
  ((match-lambda
     [(list prefix base-or-bases suffixes ...)
      (*fix-base loc-arg ctx-arg (list prefix) base-or-bases suffixes)]) args))

(define (suffix-id #:source [loc-arg #f] #:context [ctx-arg #f] . args)
  ((match-lambda
     [(list base-or-bases suffixes ...)
      (*fix-base loc-arg ctx-arg empty base-or-bases suffixes)]) args))

(module+ test
  (define-check (check-stx-equal? stx1 stx2)
    (define stxs (list stx1 stx2))
    (apply equal? (map syntax->datum stxs))) 
  (check-stx-equal? (prefix-id "foo" "bar" #'id) #'foobarid)
  (check-stx-equal? (infix-id "foo" #'id "bar" "zam") #'fooidbarzam)
  (check-stx-equal? (suffix-id #'id "foo" "bar" "zam") #'idfoobarzam)
  (for-each check-stx-equal? (suffix-id #'(this that) "@") (list #'this@ #'that@)))


(define (syntax-srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))