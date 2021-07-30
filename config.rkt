#lang racket/base

(require racket/contract)
(provide config/c check-config)

(define-syntax-rule (config/c [key vpred] ...)
  (make-contract
   #:name '(config/c [key vpred] ...)
   #:first-order
   (lambda (x) (hash? x))
   #:projection
   (lambda (b)
     (compose
      (let ([check-val ((contract-projection vpred) b)])
        (lambda (x)
          (unless (hash? x)
            (raise-blame-error
             b x
             '(expected "hash?" given "~e")
             x))
          (unless (hash-has-key? x 'key)
            (raise-blame-error
             b x
             '(expected "hash with key ~e" given "~e")
             'key x)) ...
          (check-val (hash-ref x 'key))
          x)) ...))))

(define (check-config predicate config)
  (contract predicate config
            'config 'config
            'config #f))
