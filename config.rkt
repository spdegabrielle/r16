#lang racket/base

(require racket/contract (for-syntax syntax/parse racket/base racket/syntax))
(provide config/c check-config)

(define-syntax (config/c stx)
  (syntax-parse stx
    [(_ [key vpred] ...)
     (syntax/loc stx (config/c [key vpred] ... #:optional))]
    [(_ [key vpred] ... #:optional [okey ovpred] ...)
     (define/with-syntax name stx)
     (quasisyntax/loc stx
       (make-contract
        #:name 'name
        #:first-order hash?
        #:projection
        (lambda (b)
          (compose
           (let ([check-val ((contract-projection vpred) (blame-add-context b (format "key ~s of" 'key)))])
             (lambda (x)
               (unless (hash-has-key? x 'key)
                 (raise-blame-error
                  b x
                  '(expected "hash with key ~e" given "~e")
                  'key x)) ...
               (check-val (hash-ref x 'key))
               x)) ...
           (let ([check-val ((contract-projection ovpred) (blame-add-context b (format "key ~s of" 'okey)))])
             (lambda (x)
               (when (hash-has-key? x 'okey)
                 (check-val (hash-ref x 'okey)))
               x)) ...
           (lambda (x)
             (unless (hash? x)
               (raise-blame-error
                b x
                '(expected "hash?" given "~e")
                x))
             x)))))]))

(define (check-config predicate config)
  (contract predicate config
            'config 'config
            'config #f))
