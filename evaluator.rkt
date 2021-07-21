#lang racket

(require racket/contract racket/sandbox syntax/strip-context threading)

(provide
 definitions?
 (contract-out
  [run (string? definitions? predicate/c . -> . any)]))

(define definitions? (cons/c (listof (cons/c symbol? any/c))
                             (listof module-path?)))

(define (init-evaluator)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-eval-limits '(30 20)]
                 [sandbox-propagate-exceptions #f])
    (make-evaluator 'racket)))

(define (code->exprs code)
  (parameterize ([read-accept-lang #t] [read-accept-reader #t])
    (sequence->list (in-producer read-syntax eof 'trick (open-input-string code)))))

(define (run code definitions pass-out?)
  (define (run-evaluation)
    ;; read and evaluate the code, yielding the results to be passed out
    ;; (to be called in the sandbox context)

    ;; in case it gets overridden, though sandbox-lib doesn't actually take this precaution
    ;; https://github.com/racket/racket/blob/6901f8d5511fd45516aa0a85ae070292e64226cc/pkgs/sandbox-lib/racket/sandbox.rkt#L705
    (define eval-f (current-eval))
    (define (ev expr)
      (parameterize ([current-eval eval-f])
        (eval expr)))

    (define exprs (code->exprs code))

    (define (not-module)
      ;; introduce definitions into the namespace dynamically
      (for ([mod (in-list (cdr definitions))])
        (namespace-require mod))
      (for ([def (in-list (car definitions))])
        (namespace-set-variable-value! (car def) (cdr def) #t))

      ;; evaluate `(begin exprs ...)' repl-style
      (if (null? exprs)
          null
          (let loop ([exprs exprs])
            (define expr #`(#%top-interaction . #,(car exprs)))
            (if (null? (cdr exprs))
                (call-with-values (thunk (ev expr)) list)
                (begin
                  (ev expr)
                  (loop (cdr exprs)))))))

    ;; there are two options for evaluating the code:
    ;; - the code is a series of top level expressions:
    ;;   - in this case, just evaluate them like `(begin exprs ...)'
    ;;     see `not-module' above
    ;; - the code is a module:
    ;;   - this case is a bit more complicated, since we want
    ;;     r16 definitions as well, see below
    (define raw-results
      (call/cc
       (lambda (return)
         ;; sorry for the imperative-style return
         (unless (= (length exprs) 1) (return (not-module)))

         (define (literal-identifier=? a b) ;; to match `module' better
           (or (free-identifier=? a b)
               (eq? (syntax-e a) (syntax-e b))))

         (syntax-case* (car exprs) (module) literal-identifier=?
           [(module modname lang body ...)
            (syntax-case definitions () ;; extract definitions for pattern matching
              [(((varname . varval) ...) . (modules ...))
               (begin
                 ;; define a module that provides all of the sandbox definitions
                 ;; in addition to those of the requested lang
                 (ev #'(module sandbox-language racket/base
                         ;; require and provide any defined modules
                         (provide (all-from-out modules ...))
                         (require modules ...)

                         ;; define and provide any defined variables
                         (provide varname ...)
                         (define-values (varname ...) (values 'varval ...))

                         ;; require and provide the lang of the module that was read
                         (#%provide (all-from lang))
                         (require lang)))

                 ;; then evaluate the module with the new language
                 (define trick-module
                   #'(module modname 'sandbox-language
                       body ...))
                 (ev (replace-context trick-module trick-module))

                 ;; to actually pass any values out, `r16-main' can be provided,
                 ;; otherwise the module is just instantiated for side-effects

                 (define qmodname (syntax->datum #''modname))

                 (define main
                   (dynamic-require
                    qmodname 'r16-main
                    (thunk (dynamic-require qmodname #f))))

                 (if (and (procedure? main)
                          (procedure-arity-includes? main 0))
                     (call-with-values main list)
                     (list main)))]

              [_ (not-module)])]))))

    (define (pass-out-results results)
      (for/list ([result (in-list results)]
                 #:when (not (void? result)))
        (if (pass-out? result)
            result
            (~a result))))

    (apply values (pass-out-results raw-results)))

  (parameterize ([current-environment-variables (make-environment-variables)])
    (define evaluator (init-evaluator))
    (define results
      (~> (thunk (call-in-sandbox-context evaluator run-evaluation))
          (call-with-values _ list)
          ;; call-in-sandbox-context returns void if there's an exception
          (filter-not void? _)))
    (define stdout (get-output evaluator))
    (define stderr (get-error-output evaluator))
    (kill-evaluator evaluator)
    (apply values
           `(,stdout
             ,@results
             ,(when (non-empty-string? stderr)
                (string-append "\n:warning: stderr:\n" stderr))))))
