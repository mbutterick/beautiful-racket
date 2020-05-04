#lang info

(define collection 'multi)

(define version "1.5")

;; base v6.7 dependency needs to be called 6.6.0.900 
;; due to strange little bug in `raco pkg install`
(define deps '(["base" #:version "6.6.0.900"]))

(define build-deps '("rackunit-lib"))
