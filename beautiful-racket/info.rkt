#lang info
(define collection 'multi)

(define version "1.6")

;; base v6.7 dependency needs to be called 6.6.0.900 
;; due to strange little bug in `raco pkg install`
(define deps '(["base" #:version "6.6.0.900"]
               "beautiful-racket-lib"
               "beautiful-racket-demo"))

(define build-deps '("gui-doc"
                     "gui-lib"
                     "at-exp-lib"
                     "br-parser-tools-doc"
                     "racket-doc"
                     "scribble-lib"))

(define implies '("beautiful-racket-lib"
                  "beautiful-racket-demo"))