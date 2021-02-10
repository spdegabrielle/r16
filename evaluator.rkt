#lang racket

(require racket/contract racket/sandbox)

(provide
  definitions?
  (contract-out
   (run (-> string? definitions? string?))))

(define definitions? (listof (cons/c symbol? any/c)))

(define char-cap 2000)
(define slice-size 30)

(define (truncate-string str cap)
  (let ([len (string-length str)])
    (if (> len cap)
      (let* ([slicepos (- cap slice-size)] [restsize (- len slicepos)])
        (format "~a... [~a more characters]" (substring str 0 slicepos) restsize))
      str)))

(define (format-response results stdout stderr)
  (truncate-string
    (string-append
      stdout
      (string-join
        (map ~a (filter (negate void?) results))
        "\n")
      (if (non-empty-string? stderr)
          (string-append "\n:warning: stderr:\n" stderr)
          ""))
    char-cap))


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
    (format-response results stdout stderr)))
