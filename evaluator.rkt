#lang racket

(require racket/contract racket/sandbox)

(provide
  definitions?
  (contract-out
   (run (-> string? definitions? string?))))

(define definitions? (listof (cons/c symbol? any/c)))

(define (format-response value stdout stderr)
  (format "~a~a~a"
          (if (void? value) "" value)
          stdout
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
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-propagate-exceptions #f])
    (make-evaluator
     'racket
     (make-definition definitions)
     #:requires '(threading))))

(define (run code definitions)
  (let* ((evaluator (init-evaluator definitions))
         (result (evaluator code))
         (stdout (get-output evaluator))
         (stderr (get-error-output evaluator)))
    (kill-evaluator evaluator)
    (format-response result stdout stderr)))
