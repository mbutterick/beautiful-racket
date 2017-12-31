#lang br/quicklang

(define (read-syntax path port)
  (define args (port->lines port))
  (define handle-datums (format-datums '(handle ~a) args))
  (define module-datum `(module stacker-mod stacker-demo/stacker
                          ,@handle-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? * arg) (equal? + arg) (equal? - arg) (equal? / arg))
     ; This will ensure that stacker can be extended to support - and / operations while keeping the RPN notation
     (define operand2 (pop-stack!))
     (define operand1 (pop-stack!))
     (define op-result (arg operand1 operand2)) 
     (push-stack! op-result)]))
(provide handle)

(provide + * - /)
