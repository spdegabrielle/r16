#lang racket/base

(require racket/contract (for-syntax racket/base syntax/parse))

(provide result/c ok? err? result-case
         error/c)

;; represents a fallible computation
;; (cons 'ok ok-value) on success
;; (cons 'err err-value) on failure
(define-syntax (result/c stx)
  (syntax-parse stx
    [(_ ok-ctc:expr err-ctc:expr)
     (with-syntax ([name stx])
       (syntax/loc stx
         (rename-contract
          (or/c (cons/c 'ok ok-ctc)
                (cons/c 'err err-ctc))
          'name)))]))

;; represents an error, a message tagged with a type
;; (cons message . case)
(define-syntax (error/c stx)
  (syntax-parse stx
    [(_ (tag:id others ...) ...)
     (with-syntax ([name stx])
       (syntax/loc stx
         (rename-contract
          (cons/c string?
                  (or/c (list/c 'tag others ...)
                        ...))
          'name)))]))

(define (ok? x) (eq? 'ok (car x)))
(define (err? x) (eq? 'err (car x)))
(define (result-case if-ok if-err x)
  ((if (ok? x) if-ok if-err) (cdr x)))
