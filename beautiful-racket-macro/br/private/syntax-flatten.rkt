#lang racket/base
(require racket/list)
(provide (all-defined-out))

(define (syntax-flatten stx)
  (let* ([stx-unwrapped (syntax-e stx)]
         [maybe-pair (and (pair? stx-unwrapped) (flatten stx-unwrapped))])
    (if maybe-pair
        (append-map syntax-flatten maybe-pair)
        (list stx))))

(module+ test
  (require rackunit)
  (check-equal? (map syntax->datum (syntax-flatten #'(let ([x 42])
                                                       (* x y)))) '(let x 42 * x y))
  (check-equal? (map syntax->datum (syntax-flatten #'let)) '(let)))