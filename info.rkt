#lang info
(define collection "r16")
(define version "0.1")
(define deps '("base"
               "dbg"
               "racket-cord"
               "sandbox-lib"
               "slideshow-lib"
               "threading-lib"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-lib"
                     "threading-doc"))
(define test-omit-paths '("presentation"))
(define scribblings '(("scribblings/r16.scrbl" ())))
