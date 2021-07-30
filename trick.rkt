#lang racket/base

;; #lang r16/trick <trick>

(module reader racket
  (provide (rename-out [read-syntax-trick read-syntax]
                       [read-trick read]))
  (define (read-trick [in (current-input-port)])
    (syntax->datum (read-syntax-trick 'program in)))
  (define (read-syntax-trick src in)
    (define trick (read in))
    (define string-args (string-trim (port->string in)))
    #`(module trick-module '#%kernel
        (#%provide r16-main)
        (define-values (r16-main)
          (lambda ()
            (call-trick '#,trick #,string-args))))))
