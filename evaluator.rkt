#lang racket

(require racket/contract racket/sandbox)

(provide
  definitions?
  (contract-out
   (run (-> string? definitions? any))))

(define definitions? (listof (cons/c symbol? any/c)))

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
         (results (call-with-values (thunk (evaluator code)) list))
         (stdout (get-output evaluator))
         (stderr (get-error-output evaluator)))
    (kill-evaluator evaluator)
    (apply values
           `(,stdout
             ,@results
             ,(if (non-empty-string? stderr)
                  (string-append "\n:warning: stderr:\n" stderr)
                  (void))))))
