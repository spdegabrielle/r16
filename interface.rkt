#lang racket/base

(require
 racket/class
 racket/contract
 "result.rkt"
 (only-in "evaluator.rkt" definitions? run-result?)
 "common.rkt")

(provide r16-backend? r16-frontend?
         r16-backend<%> r16-frontend<%>
         current-backend current-frontend
         current-context-id
         (all-from-out "result.rkt"))

;; an r16 frontend
(define r16-frontend<%>
  (interface ()
    ;; get whether a value should be passed out of the sandbox
    [response?          (->m any/c boolean?)]

    ;; get a function that introduces new bindings to the base sandbox definitions
    ;;
    ;; this function may be called repeatedly within the sandbox,
    ;; so use this to initialise any state,
    ;; and to close over any parameters
    [get-enrich-context (->m (-> #;base definitions?
                                 #;trick (or/c trick? #f)
                                 #;args string?
                                 #;parent-ctx (or/c definitions? #f)
                                 definitions?))]

    ;; get whether there are sufficient permissions to modify a trick
    [can-modify?        (->m trick? boolean?)]

    ;; start this frontend and block on it
    [start (->m any)]))

;; the r16 backend
(define r16-backend<%>
  (interface ()
    ;; evaluate a code snippet, returning either an error message or a run result
    [evaluate (#;code string? . ->m .
               (result/c run-result?
                         (error/c)))]

    ;; call a trick with arguments, returning either an error or a run result
    [call     (#;trick string? #;args string? . ->m .
               (result/c run-result?
                         (error/c (no-such-trick))))]

    ;; delete a trick, returning an error or success message
    [delete   (#;trick string? . ->m .
               (result/c string?
                         (error/c (no-such-trick)
                                  (missing-permissions))))]

    ;; register a trick, returning an error or success message
    [register (#;trick string? #;code string?
               #;author string? #;timestamp exact-integer? . ->m .
               (result/c string?
                         (error/c (needs-body)
                                  (missing-permissions))))]

    ;; update a trick, returning an error or success message
    [update   (#;trick string? #;code string? . ->m .
               (result/c string?
                         (error/c (no-such-trick)
                                  (needs-body)
                                  (missing-permissions))))]

    ;; look up a trick by name
    [lookup   (#;trick string? . ->m . (or/c trick? #f))]

    ;; list the registered tricks, sorted by invocation count
    [popular  (->m (listof (cons/c string? trick?)))]

    ;; save the database, returning the status
    [save     (->m (or/c 'success 'unchanged exn:fail?))]

    ;; get version info
    [about    (->m string?)]

    ;; get operational stats
    [stats    (->m string?)]))

(define (r16-frontend? x)
  (is-a? x r16-frontend<%>))

(define (r16-backend? x)
  (is-a? x r16-backend<%>))

;; The active frontend. Used by the backend to know what frontend it's receiving a
;; request from. Transient, and should be re-parameterized by each frontend to itself
;; for each incoming message. Consequently, this should always be set before calling
;; any backend method.
(define/contract current-frontend
  (parameter/c (or/c r16-frontend? #f))
  (make-parameter #f))

;; Long-term holder for the backend.
(define/contract current-backend
  (parameter/c (or/c r16-backend? #f))
  (make-parameter #f))

(define/contract current-context-id
  (parameter/c string?)
  (make-parameter "anonymous"))
