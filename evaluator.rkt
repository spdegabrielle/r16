#lang racket/base

(require
 racket/contract
 (only-in racket/format ~a)
 (only-in racket/function thunk)
 (only-in racket/list filter-not)
 racket/sandbox
 (only-in racket/sequence sequence->list)
 (only-in racket/string non-empty-string?)
 (only-in syntax/strip-context replace-context)
 threading)

(provide
 definitions?
 run-result?
 (contract-out
  [run (string? definitions? predicate/c . -> . run-result?)]
  [run-result-stdout (run-result? . -> . string?)]
  [run-result-stderr (run-result? . -> . (or/c non-empty-string? #f))]
  [run-result-results (run-result? . -> . (listof any/c))]
  [run-result (string? (or/c non-empty-string? #f) (listof any/c) . -> . run-result?)]))

(define definitions? (cons/c (listof (cons/c symbol? any/c))
                             (listof module-path?)))

(define recursive-sandbox-call? (make-parameter #f))

(define (unique-sym name)
  (string->uninterned-symbol (symbol->string name)))

;; in case it gets overridden, though sandbox-lib doesn't actually take this precaution
;; https://github.com/racket/racket/blob/6901f8d5511fd45516aa0a85ae070292e64226cc/pkgs/sandbox-lib/racket/sandbox.rkt#L705
(define ev
  (let ([eval-f (current-eval)])
    (lambda (expr)
      (parameterize ([current-eval eval-f])
        (eval expr)))))

(define (make-module-namespace name)
  (define sym (unique-sym name))
  (ev `(module ,sym racket))
  (dynamic-require `',sym #f)
  (module->namespace `',sym))

(define (init-evaluator)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-propagate-exceptions #f]
                 [sandbox-make-environment-variables make-environment-variables])
    (make-evaluator 'racket)))

(define (code->exprs code)
  (define port (open-input-string code))
  (port-count-lines! port)
  (parameterize ([read-accept-lang #t] [read-accept-reader #t])
    (sequence->list (in-producer read-syntax eof 'trick port))))

(struct run-result (stdout stderr results) #:transparent)

(define (run code definitions pass-out?)
  (define (run-evaluation)
    ;; read and evaluate the code, yielding the results to be passed out
    ;; (to be called in the sandbox context)

    (recursive-sandbox-call? #t)

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
            (identifier? #'modname)
            (let ()
              (define varnames (map car (car definitions)))
              (define (eval-quote form) (cons '#%datum form))
              (define varvals (map (compose eval-quote cdr) (car definitions)))
              (define modules (cdr definitions))
              (define sandbox-language (unique-sym 'sandbox-language))
              (define modname-sym (unique-sym (syntax-e #'modname)))
              (define module-name (datum->syntax #'modname modname-sym))
              ;; define a module that provides all of the sandbox definitions
              ;; in addition to those of the requested lang
              (ev #`(module #,sandbox-language racket/base
                      ;; require and provide any defined modules
                      (provide (all-from-out #,@modules))
                      (require #,@modules)

                      ;; define and provide any defined variables
                      (provide #,@varnames)
                      (define-values (#,@varnames) (values #,@varvals))

                      ;; require and provide the lang of the module that was read
                      (#%provide (all-from lang))
                      (require lang)))

              ;; then evaluate the module with the new language
              (define trick-module
                #`(module #,module-name '#,sandbox-language
                    body ...))
              (ev (replace-context trick-module trick-module))

              ;; to actually pass any values out, `r16-main' can be provided,
              ;; otherwise the module is just instantiated for side-effects

              (define qmodname (list 'quote modname-sym))

              (define main
                (dynamic-require
                 qmodname 'r16-main
                 (thunk (dynamic-require qmodname #f))))

              (if (and (procedure? main)
                       (procedure-arity-includes? main 0))
                  (call-with-values main list)
                  (list main)))]

           [_ (not-module)]))))

    (define (pass-out-results results)
      (for/list ([result (in-list results)]
                 #:when (not (void? result)))
        (if (pass-out? result)
            result
            (~a result))))

    (apply values (pass-out-results raw-results)))

  (define-values (stdout results stderr)
    (if (recursive-sandbox-call?)
        (let ()
          (define stdout-port (open-output-string))
          (define stderr-port (open-output-string))
          (define results
            (parameterize ([current-output-port stdout-port]
                           [current-error-port stderr-port]
                           [current-namespace (make-module-namespace 'recursive-trick)])
              (call-with-values run-evaluation list)))
          (values (get-output-string stdout-port)
                  results
                  (get-output-string stderr-port)))

        (let ()
          (define evaluator (init-evaluator))
          (define results
            (~> (thunk (call-in-sandbox-context evaluator run-evaluation))
                (call-with-values _ list)
                ;; call-in-sandbox-context returns void if there's an exception
                (filter-not void? _)))
          (define stdout (get-output evaluator))
          (define stderr (get-error-output evaluator))
          (kill-evaluator evaluator)
          (values stdout results stderr))))

  (run-result stdout (and (non-empty-string? stderr) stderr) results))

(module+ test
  (require
   (only-in racket/class is-a?)
   (only-in racket/format ~s)
   rackunit)

  (define empty-defs '(()))
  (define (any? _ignored) #t)

  (test-case "Empty Test"
    (check-equal?
     (run "" empty-defs any?)
     (run-result "" #f '())))

  (test-case "Single Expr Test"
    (check-equal?
     (run "1" empty-defs any?)
     (run-result "" #f '(1)))
    (check-equal?
     (run (~s `(+ 1 2)) empty-defs any?)
     (run-result "" #f '(3)))
    (check-equal?
     (run (~s `(values 1 2 3)) empty-defs any?)
     (run-result "" #f '(1 2 3))))

  (test-case "Multiple Expr Test"
    (check-equal?
     (run "1 2 3" empty-defs any?)
     (run-result "" #f '(3)))
    (check-equal?
     (run (~s '(define x 100)
              'x)
          empty-defs any?)
     (run-result "" #f '(100))))

  (test-case "Stdio Test"
    (check-equal?
     (run (~s `(display "stdout test"))
          empty-defs any?)
     (run-result "stdout test" #f '()))
    (check-equal?
     (run (~s `(display "stderr test" (current-error-port)))
          empty-defs any?)
     (run-result "" "stderr test" '())))

  (test-case "Module Test"
    (check-equal?
     (run (~s `(module test racket
                 1
                 2
                 3))
          empty-defs any?)
     (run-result "1\n2\n3\n" #f '()))
    (check-equal?
     (run "#lang racket\
           1 2 3"
          empty-defs any?)
     (run-result "1\n2\n3\n" #f '()))
    (check-equal?
     (run (string-append
           "#lang racket\n"
           (~s `(provide r16-main)
               `(define r16-main 100)))
          empty-defs any?)
     (run-result "" #f '(100)))
    (check-equal?
     (run (string-append
           "#lang racket\n"
           (~s `(provide r16-main)
               `(define (r16-main) (values 1 2 3))))
          empty-defs any?)
     (run-result "" #f '(1 2 3))))

  (test-case "Binding Test"
    (define xv (box "box"))
    (let ()
      (define rr (run "x" `(((x . ,xv))) any?))
      (check-false (run-result-stderr rr))
      (check-equal? (~> rr run-result-results car) xv))
    (let ()
      (define rr
        (run (~s `(module test racket
                    (provide r16-main)
                    (define (r16-main) x)))
             `(((x . ,xv))) any?))
      (check-false (run-result-stderr rr))
      ;; not necessarily eq? unfortunately
      (check-equal? (~> rr run-result-results car) xv)))

  (define ((run-fn defs) expr)
    (define rr (run expr defs any?))
    (display (run-result-stdout rr))
    (cond [(run-result-stderr rr)
           => (lambda (stderr) (display stderr (current-error-port)))])
    (apply values (run-result-results rr)))

  (test-case "Recursive Call Test"
    (check-equal?
     (run (~s `(run (~s `(+ 1 2 3))))
          `(((run . ,(run-fn empty-defs))))
          any?)
     (run-result "" #f '(6))))

  (require racket/draw)

  (test-case "Reused Instantation Test"
    ;; bitmap% IN the sandbox is still different to that outside
    (check-false
     (~> (run
          (~s `(require racket/draw)
              `(make-bitmap 1 1))
          empty-defs
          any?)
         run-result-results
         car
         (is-a? _ bitmap%)))

    (check-equal?
     (run
      (~s `(require racket/draw)
          `(define bmp
             (run (~s `(require racket/draw)
                      `(make-bitmap 1 1))))
          `(is-a? bmp bitmap%))
      `(((run . ,(run-fn empty-defs))))
      any?)
     (run-result "" #f '(#t)))

    (check-equal?
     (run
      (~s `(require racket/draw)
          `(define bmp
             (run (~s `(require racket/draw)
                      `(run (~s `(require racket/draw)
                                `(make-bitmap 1 1))))))
          `(is-a? bmp bitmap%))
      `(((run . ,(run-fn `(((run . ,(run-fn empty-defs))))))))
      any?)
     (run-result "" #f '(#t))))

  (test-case "Namespace Hygiene Test"
    (check-equal?
     (run (~s `(define x 'x-value)
              `(foil-namespace)
              `x)
          `(((foil-namespace
              .
              ,(thunk
                (run (~s `(for ([sym (in-list (namespace-mapped-symbols))])
                            (namespace-undefine-variable! sym)))
                     empty-defs any?)))))
          any?)
     (run-result "" #f '(x-value)))

    (check-equal?
     (let ()
       (define noop (~s `(void)))
       (define noop-defs `(((args . "no-args"))))
       (define foo (~s `(module foo racket
                          (displayln args)
                          (run ,noop)
                          (displayln args))))
       (define foo-defs `(((run . ,(run-fn noop-defs))
                           (args . "args"))))
       (define app (~s `(run ,foo)))
       (define app-defs `(((run . ,(run-fn foo-defs)))))
       (run app app-defs any?))
     (run-result "args\nargs\n" #f '()))

    (check-equal?
     (let ()
       (define noop (~s `(void)))
       (define noop-defs `(((args . "no-args"))))
       (define foo (~s `(displayln args)
                       `(run ,noop)
                       `(displayln args)))
       (define foo-defs `(((run . ,(run-fn noop-defs))
                           (args . "args"))))
       (define app (~s `(run ,foo)))
       (define app-defs `(((run . ,(run-fn foo-defs)))))
       (run app app-defs any?))
     (run-result "args\nargs\n" #f '()))

    (check-equal?
     (let ()
       (define selfrec-module
         (~s `(module selfrec racket
                (define x value)
                (displayln x)
                (recur)
                (displayln x))))
       (define selfrec-defs
         (for/fold ([defs `(((recur . ,void)
                             (value . top)))])
                   ([i (in-range 5)])
           `(((recur . ,(thunk ((run-fn defs) selfrec-module)))
              (value . ,i)))))
       (run selfrec-module selfrec-defs any?))
     (run-result (~a 4 3 2 1 0 'top
                     'top 0 1 2 3 4 ""
                     #:separator "\n") #f '())))

  (test-case "Exn Test"
    (check-not-false
     (run-result-stderr
      (run (~s `(raise "err"))
           empty-defs any?)))
    (check-not-false
     (run-result-stderr
      (run (~s `(run (~s `(raise "err"))))
           `(((run . ,(run-fn empty-defs))))
           any?)))))
