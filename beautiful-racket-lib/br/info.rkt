#lang info

;; for unknown reason "indent.rkt" 
;; started causing CI failures
;; consistently on 6.7, 7.7CS, 7.8CS, 7.9CS
;; I assume it has something to do with the fact that 
;; it imports `framework` and `racket/gui`, 
;; OTOH why does it fail in these?
(define test-omit-paths '("indent.rkt"))