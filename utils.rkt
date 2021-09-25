#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide thread-loop)

(define-syntax (thread-loop stx)
  (syntax-parse stx
    [(_ body:expr ...)
     (syntax/loc stx
       (let ()
         (define (loop)
           body ...
           (loop))
         (thread loop)))]))
