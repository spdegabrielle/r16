#lang racket

(define definitions? (listof (cons/c symbol? any/c)))

(require racket/contract racket/sandbox)
(provide
  (contract-out
    (definitions? (-> any/c boolean?))
    (run (-> string? definitions? string?))))

(define (format-response value stderr)
  (format "~a~a"
          (if (void? value) "" value)
          (if (non-empty-string? stderr)
              (string-append "\n:warning: stderr:\n" stderr)
              "")))

; Evaluate a form, then quote it
(define (eval-quote form) `',form)

(define (make-definition definitions)
  (when definitions
    `(define-values
      ,(map car definitions)
      (values ,@(map (compose1 eval-quote cdr) definitions)))))

(define (init-evaluator definitions)
  (parameterize ([sandbox-output #f]
                 [sandbox-error-output 'string]
                 [sandbox-propagate-exceptions #f])
    (make-evaluator
     'racket
     (make-definition definitions)
     #:requires (list 'threading))))

(define (run code definitions)
  (let* ((evaluator (init-evaluator definitions))
         (result (evaluator code))
         (stderr (get-error-output evaluator)))
    (kill-evaluator evaluator)
    (format-response result stderr)))
