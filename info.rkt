#lang info
(define collection "r16")
(define version "0.1")
(define deps '("base"
               "dbg"
               "racket-cord"
               "sandbox-lib"
               "slideshow-lib"
               "srfi-lite-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '("racket-doc"
                     "draw-lib" ;; evaluator unit tests. TODO try to get rid of this
                     "rackunit-lib"
                     "scribble-lib"
                     "threading-doc"))
(define compile-omit-paths '("presentation"))
(define test-omit-paths '("presentation"))
(define scribblings '(("scribblings/r16.scrbl" ())))
(define license 'MIT)
