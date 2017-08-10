#lang racket/base
(require "syntax-flatten.rkt")
(provide all-...-follow-wildcards generate-literals)

(define (literal-identifier? pat-datum)
  (and (symbol? pat-datum)
       (not (memq pat-datum '(... _))) ; isn't a reserved identifier
       (let ([pat-str (symbol->string pat-datum)])
         (or (not (regexp-match #rx"[A-Z]" pat-str)) ; either doesn't contain at least one uppercase letter ...
             (not (equal? (string-upcase pat-str) pat-str)))))) ;...  or doesn't contain all uppercase letters

(define (wildcard-identifier? pat-datum)
  (and (symbol? pat-datum)
       (not (literal-identifier? pat-datum))
       (not (memq pat-datum '(... _)))))

;; generate literals for any symbols that are not ... or _ and not IN_CAPS
(define (generate-literals pats)
  (for*/list ([pat-arg (in-list (syntax-flatten pats))]
              [pat-datum (in-value (syntax->datum pat-arg))]
              #:when (literal-identifier? pat-datum))
    pat-arg))

(define (all-...-follow-wildcards pats)
  (define prev-datum (box #f))
  (and
    (for*/and ([pat-arg (in-list (syntax-flatten pats))]
               [pat-datum (in-value (syntax->datum pat-arg))])
      ;; OK if there's no previous datum,
      (and
        (when (eq? pat-datum '...)
          (wildcard-identifier? (unbox prev-datum)))
        (set-box! prev-datum pat-datum)))
    #true))

(module+ test
  (require rackunit)
  (check-equal? (map syntax->datum (generate-literals #'(foo 42 BAR _ (... ...) bar <=> 3Bar 3bar))) '(foo bar <=> 3Bar 3bar))

  (test-case "wildcard-identifier?"
    (check-true (wildcard-identifier? 'FOO))
    (check-true (wildcard-identifier? 'TOPPING))

    (check-false (wildcard-identifier? 'piZZa))
    (check-false (wildcard-identifier? 'please)))

  (test-case "all-...-follow-wildcards"
    (check-true (all-...-follow-wildcards #'()))
    (check-true (all-...-follow-wildcards (datum->syntax #f '(a b))))
    (check-true (all-...-follow-wildcards (datum->syntax #f '(a b C ...))))

    (check-false (all-...-follow-wildcards (datum->syntax #f '(...))))
    (check-false (all-...-follow-wildcards (datum->syntax #f '(a ...))))
    (check-false (all-...-follow-wildcards (datum->syntax #f '(A ... b ...))))))
