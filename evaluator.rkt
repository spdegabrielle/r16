#lang racket

(require racket/contract racket/sandbox racket/exn syntax/strip-context)

(provide
 definitions?
 (contract-out
  [run (string? definitions? predicate/c . -> . any)]))

(define definitions? (cons/c (listof (cons/c symbol? any/c))
                             (listof module-path?)))

; Evaluate a form, then quote it
(define (eval-quote form) `',form)

(define (make-definition definitions)
  `(begin
     (require ,@(cdr definitions))
     (define-values
       ,(map car (car definitions))
       (values ,@(map (compose1 eval-quote cdr) (car definitions))))))

(define (literal-identifier=? a b)
  (or (free-identifier=? a b) (eq? (syntax-e a) (syntax-e b))))

(define default-sandbox-reader (sandbox-reader))

(define ((language-morph-reader definitions) value)
  (let ([exprs (parameterize ([read-accept-reader #t]
                              [read-accept-lang #t])
                 (default-sandbox-reader value))])
    (or
     (and
      (= (length exprs) 1)
      (syntax-case* (car exprs) (module) literal-identifier=?
        [(module modname lang body ...)
         (let ([full-stx
                #`(begin
                    ;; define a language that provides the sandbox definitions,
                    ;; but also all of the lang's definitions
                    (module sandbox-language racket
                      (provide (all-defined-out)
                               #,@(map (curry list 'all-from-out)
                                       (cdr definitions)))
                      (#%provide (all-from lang))
                      #,(make-definition definitions)
                      ;; require everything else at the end
                      (require lang))
                    ;; define the module with the new language
                    (module program 'sandbox-language
                      body ...)
                    ;; instantiate the module
                    ;; if it provides r16-main, return it
                    (define main
                      (dynamic-require ''program 'r16-main
                                       (thunk (dynamic-require ''program #f))))
                    (if (and (procedure? main)
                             (procedure-arity-includes? main 0))
                        (main)
                        main))])
           ;; replace the lexical context,
           ;; allowing for the sandbox definitions to be used
           (list (replace-context full-stx full-stx)))]
        [_ #f]))
     (cons (make-definition definitions) exprs))))

(define (init-evaluator definitions)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-eval-limits '(30 20)]
                 [sandbox-propagate-exceptions #f]
                 [sandbox-reader (language-morph-reader definitions)])
    (make-evaluator 'racket)))

(define (run code definitions pass-out?)
  (parameterize ([current-environment-variables (make-environment-variables)])
    (let* ([evaluator (init-evaluator definitions)]
           [results
            (call-with-values
             (thunk
              (with-handlers ([(const #t) identity])
                (evaluator code)))
             (lambda results
               (call-in-sandbox-context
                evaluator
                (thunk
                 (for/list ([result (in-list results)]
                            #:when (not (void? result)))
                   (if (pass-out? result)
                       result
                       (with-handlers
                         ([(const #t)
                           (lambda (e)
                             (with-handlers ([(const #t) (const "#<errored>")])
                               ((error-display-handler)
                                (exn-message e)
                                e)))])
                         (~a result))))))))]
           [stdout (get-output evaluator)]
           [stderr (get-error-output evaluator)])
      (kill-evaluator evaluator)
      (apply values
             `(,stdout
               ,@results
               ,(if (non-empty-string? stderr)
                    (string-append "\n:warning: stderr:\n" stderr)
                    (void)))))))
