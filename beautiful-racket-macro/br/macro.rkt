#lang racket/base
(require racket/function
         (for-syntax racket/base
                     syntax/parse
                     "private/generate-literals.rkt"
                     syntax/define))
(provide (all-defined-out)
         (for-syntax with-shared-id))

(module+ test (require rackunit))

(begin-for-syntax
  ;; expose the caller context within br:define macros with syntax parameter
  (require (for-syntax racket/base) racket/stxparam)
  (provide caller-stx)
  (define-syntax-parameter caller-stx (λ(stx) (error 'caller-stx-not-parameterized))))

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


(module+ test
  (define-cases f
    [(_ arg) (add1 arg)]
    [(_ arg1 arg2) (+ arg1 arg2)]
    [(_ . any) 'boing])
  (check-equal? (f 42) 43)
  (check-equal? (f 42 5) 47)
  (check-equal? (f 42 5 'zonk) 'boing)

  (define-cases f-one-arg
    [(_ arg) (add1 arg)])
  (check-exn exn:fail:contract:arity? (λ _ (f-one-arg 1 2 3))))


(define-syntax-rule (debug-define-macro (ID . PAT-ARGS) BODY)
  (define-macro (ID . PAT-ARGS)
    #`(begin
        (for-each displayln
                  (list
                   (format "input pattern = #'~a" '#,'(ID . PAT-ARGS))
                   (format "output pattern = #'~a" (cadr '#,'BODY))
                   (format "invoked as = ~a" (syntax->datum #'(ID . PAT-ARGS)))
                   (format "expanded as = ~a" '#,(syntax->datum BODY))
                   (format "evaluated as = ~a" #,BODY)))
        #,BODY)))


(module+ test
  (require racket/port)
  (parameterize ([current-output-port (open-output-nowhere)])
    (check-equal? (let ()
                    (debug-define-macro (foo X Y Z)
                                        #'(apply + (list X Y Z)))
                    (foo 1 2 3)) 6)  
    (check-equal? (let ()
                    (debug-define-macro (foo X ...) #'(apply * (list X ...)))
                    (foo 10 11 12)) 1320)))


(begin-for-syntax
  (begin-for-syntax
    (require (for-syntax racket/base))
    (define-syntax-rule (make-shared-syntax-macro caller-stx)
      #'(syntax-rules stx
          [(_ form)
           #'(datum->syntax caller-stx (if (syntax? form)
                                           (syntax-e form)
                                           form))]))))

(module+ test
  (define-macro (dirty-maker ARG)
    (with-syntax ([dirty-bar (datum->syntax caller-stx 'dirty-bar)])
      #'(define dirty-bar (* ARG 2))))
  (dirty-maker 42)
  (check-equal? dirty-bar 84))


(begin-for-syntax
  (define-syntax-rule (with-shared-id (id ...) . body)
    (with-syntax ([id (datum->syntax caller-stx 'id)] ...)
      . body)))


;; `syntax-parse` classes shared by `define-macro` and `define-macro-cases`
(begin-for-syntax
  (require syntax/parse)
  
  (define-syntax-class syntaxed-id
    #:literals (syntax quasisyntax)
    #:description "id in syntaxed form"
    (pattern ([~or syntax quasisyntax] name:id)))
  
  (define-syntax-class syntaxed-thing
    #:literals (syntax quasisyntax)
    #:description "some datum in syntaxed form"
    (pattern ([~or syntax quasisyntax] thing:expr)))

  (define-syntax-class else-clause
    #:literals (else)
    (pattern [else . body:expr]))

  (define-syntax-class transformer-func
    #:literals (lambda λ)
    (pattern ([~or lambda λ] (arg:id) . body:expr))))


(define-syntax (define-macro stx)
  (syntax-parse stx
    [(_ id:id stxed-id:syntaxed-id)
     #'(define-syntax id (make-rename-transformer stxed-id))]
    [(_ id:id func:transformer-func)
     #'(define-syntax id func)]
    [(_ id:id func-id:id)
     #'(define-syntax id func-id)]
    [(_ id:id stxed-thing:syntaxed-thing) 
     #'(define-macro id (λ (stx) stxed-thing))]
    [(_ (id:id . patargs:expr) . body:expr)
     (with-syntax ([id (syntax-property #'id 'caller 'define-macro)])
       #'(define-macro-cases id [(id . patargs) (begin . body)]))]
    [else (raise-syntax-error
           'define-macro
           "no matching case for calling pattern"
           (syntax->datum stx))]))


(define-syntax (define-macro-cases stx)
  (define (error-source stx) (or (syntax-property stx 'caller) 'define-macro-cases))
  (syntax-parse stx
    [(_ id:id)
     (raise-syntax-error (error-source #'id) "no cases given" (syntax->datum #'id))]
    [(_ id:id leading-pat:expr ... else-pat:else-clause trailing-pat0:expr trailing-pat:expr ...)
     (raise-syntax-error (error-source #'id) "`else` clause must be last" (syntax->datum #'id))]
    [(_ id:id (pat:expr . result-exprs:expr) ... else-clause:else-clause)
     (unless (ellipses-follow-wildcards-or-subpatterns? #'(pat ...))
       (raise-syntax-error (error-source #'id) "ellipsis in pattern can only appear after wildcard or subpattern" (syntax->datum stx)))
     (with-syntax ([(PAT ...) (map (λ (x) (literalize-pat x #'~literal)) (syntax->list #'(pat ...)))])
       #'(define-macro id
           (λ (stx)
             (define result
               (syntax-parameterize ([caller-stx (make-rename-transformer #'stx)])
                 (syntax-parse (syntax-case stx () [any #'any])
                   [PAT . result-exprs] ...
                   else-clause)))
             (if (syntax? result)
                 result
                 (datum->syntax #'id result)))))]
    [(_ id:id pat-clause:expr ...) ; macro without `else` clause will reach this branch
     #'(define-macro-cases id
         pat-clause ...
         [else (raise-syntax-error
                'id
                "no matching case for calling pattern"
                (syntax->datum caller-stx))])]
    [else (raise-syntax-error
           (error-source #'id)
           "no matching case for calling pattern"
           (syntax->datum stx))]))


(module+ test
  (define-macro plus (λ (stx) #'+))
  (check-equal? (plus 42) +)
  (define-macro plusser #'plus)
  (check-equal? (plusser 42) +)
  (check-equal? plusser +)
  (define-macro (times [nested ARG]) #`(* ARG ARG))
  (check-equal? (times [nested 10]) 100)
  (define-macro timeser #'times)
  (check-equal? (timeser [nested 12]) 144)
  (define-macro fortytwo #`42)
  (check-equal? fortytwo 42)
  (check-equal? (let ()
                  (define-macro (foo X)
                    (with-syntax ([zam #'+])
                      #'(zam X X))) (foo 42)) 84) 
  (begin
    (define-macro (redefine ID) #'(define ID 42))
    (redefine zoombar)
    (check-equal? zoombar 42))
  
  ;; use caller-stx parameter to introduce identifier unhygienically
  (define-macro (zam ARG1 ARG2 ARG3)
    (with-syntax ([dz (datum->syntax caller-stx 'dirty-zam)])
      #`(define dz 'got-dirty-zam)))
  
  (zam 'this 'that 42)
  (check-equal? dirty-zam 'got-dirty-zam)
  
  (define-macro (add X) #'(+ X X))
  (check-equal? (add 5) 10)
  (define-macro (add-b 9X) #'(+ 9X 9X))
  (check-equal? (add-b 5) 10)
  (define-macro-cases add-again [(_ X) #'(+ X X)])
  (check-equal? (add-again 5) 10)
  (define-macro-cases add-3rd [(_ X) #'(+ X X)])
  (check-equal? (add-3rd 5) 10)
  (define-macro add-4th #'add-3rd)
  (check-equal? (add-4th 5) 10)
  (define foo-val 'got-foo-val)
  (define (foo-func) 'got-foo-func)
  (define-macro-cases op
    [(_ "+") #''got-plus]
    [(_ ARG) #''got-something-else]
    [(_) #'(foo-func)]
    [_ #'foo-val])
  
  (check-equal? (op "+") 'got-plus)
  (check-equal? (op 42) 'got-something-else)
  (check-equal? (op) 'got-foo-func)
  (check-equal? op 'got-foo-val)
  
  (define-macro-cases elseop
    [(_ ARG) #''got-arg]
    [else #''got-else])
  
  (check-equal? (elseop "+") 'got-arg)
  (check-equal? (elseop "+" 42) 'got-else)

  (check-exn exn:fail:syntax? (λ () (expand-once #'(define-macro-cases no-cases))))

  (check-exn exn:fail:syntax? (λ () (expand-once #'(define-macro-cases badelseop
                                                     [else #''got-else]
                                                     [(_ _arg) #''got-arg]))))

  (define-macro-cases no-else-macro
    [(_ ARG) #''got-arg])
  (check-exn exn:fail:syntax? (λ _ (expand-once #'(no-else-macro 'arg1 'arg2)))))

(define-macro (define-unhygienic-macro (ID PAT ...) BODY ... STX-OBJECT)
  #'(define-macro (ID PAT ...)
      BODY ...
      (datum->syntax caller-stx (syntax->datum STX-OBJECT))))
