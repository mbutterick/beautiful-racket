#lang racket/base
(require "syntax-flatten.rkt" racket/list racket/match syntax/parse)
(provide ellipses-follow-wildcards-or-subpatterns? literalize-pat)

(define (literal-identifier? x)
  (let ([x (if (syntax? x) (syntax-e x) x)])
    (and (symbol? x)
         (not (memq x '(... _))) ; isn't a reserved identifier
         (let ([pat-str (symbol->string x)])
           (or (not (regexp-match #rx"[A-Z]" pat-str)) ; either doesn't contain at least one uppercase letter ...
               (not (equal? (string-upcase pat-str) pat-str))))))) ;...  or doesn't contain all uppercase letters


(define (wildcard-datum? x) (and (symbol? x) (not (literal-identifier? x))))


(define (literalize-pat pat [literalizer-id #'~literal])
  ;; take `literalizer-id` as explicit input from the calling site
  ;; because this is a macro helper
  ;; so hygiene is not enforced
  (let loop ([pat pat])
    (syntax-case pat ()
      [(HEAD . TAIL) (datum->syntax pat (cons (loop #'HEAD) (loop #'TAIL)))]
      [MAYBE-LIT-ID (literal-identifier? #'MAYBE-LIT-ID)
              (with-syntax ([literalizer-id literalizer-id]
                            [lit-id #'MAYBE-LIT-ID])
                #'(literalizer-id lit-id))]
      [ANY #'ANY])))


(module+ test
  (check-equal? (syntax->datum (literalize-pat #'(hello WORLD))) '((~literal hello) WORLD))
  (check-equal? (syntax->datum (literalize-pat #'(hello . WORLD))) '((~literal hello) . WORLD))
  (check-equal? (syntax->datum (literalize-pat #'(hello WORLD (... ...)))) '((~literal hello) WORLD  ...))
  (check-equal? (syntax->datum (literalize-pat #'(hello (cruel WORLD)))) '((~literal hello) ((~literal cruel) WORLD)))
  (check-equal? (syntax->datum (literalize-pat #'(hello (cruel . WORLD)))) '((~literal hello) ((~literal cruel) . WORLD))))


(define (ellipses-follow-wildcards-or-subpatterns? pat)
  (let loop ([datum (syntax->datum pat)])
    (match datum
      [(? null?) #t]
      [(cons '... _) #f]
      [(cons _ '...) #f]
      [(list head '... tail ...) (and (or (wildcard-datum? head) (pair? head))
                                      (loop head)
                                      (loop tail))]
      [(list head tail ...) (and (loop head) (loop tail))]
      [(cons x y) (loop (list x y))]
      [else #t])))


(module+ test
  (require rackunit)
  (test-case "wildcard-identifier?"
             (check-true (wildcard-datum? 'FOO))
             (check-true (wildcard-datum? 'TOPPING))
             (check-false (wildcard-datum? 'piZZa))
             (check-false (wildcard-datum? 'please)))

  (test-case "all-...-follow-wildcards"
             (check-true (ellipses-follow-wildcards-or-subpatterns? #'()))
             (check-true (ellipses-follow-wildcards-or-subpatterns? #'foo))
             (check-true (ellipses-follow-wildcards-or-subpatterns? #'(foo . bar)))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(a b))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(a b C ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((a b) ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((C D) ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((C ...) ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((C ...) ...) ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((C ...) D ...) ...))))

             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(a ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(A ... b ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((a ...) ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((a ...) ...) ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((B ...) a ...) ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((...) B ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((((...))))))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(A . ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(... A))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(... . A))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(... . ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((... . A) ...))))))
