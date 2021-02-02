#!/usr/bin/env racket
#lang racket

(require (prefix-in rc: racket-cord))
(require (prefix-in http: racket-cord/http))
(require (prefix-in ev: "evaluator.rkt"))
(require (prefix-in db: "trick-db.rkt"))

(define (strip-trim msg prefix)
  (string-trim (substring msg (string-length prefix))))

(define (message-from-bot? message)
  (and (not (null? (rc:message-author message)))
       (not (null? (rc:user-bot (rc:message-author message))))))

(define ((contextualizer client) message)
  (let ((channel (http:get-channel client (rc:message-channel-id message))))
    (and (rc:guild-channel? channel) (rc:guild-channel-guild-id channel))))

(define (make-db client filename) (db:make-trickdb (contextualizer client) filename))

(define (strip-backticks code)
  (let ([groups (regexp-match #px"```(\\w+\n)?(.+)```" code)])
    (if groups
        (caddr groups)
        code)))

(define (run-snippet client message code)
  (let ([code (strip-backticks code)]
        [text (rc:message-content message)])
    (ev:run code text)))
        
(define (register-trick client message rst)
  (~a "Registering " message))

(define (call-trick client message name)
  (~a "Calling " name))

(define (show-trick client message name)
  (~a "Showing " name))

(define command-table (list (cons "eval" run-snippet)
                            (cons "register" register-trick)
                            (cons "call" call-trick)
                            (cons "show" show-trick)))

(define (dispatch-command client message text)
  (ormap (lambda (pair)
           (let ([cmdname (car pair)]
                 [func (cdr pair)])
             (and (string-prefix? text cmdname)
                  (func client message (strip-trim text cmdname)))))
         command-table))

(define prefix "!rkt") 

(define (message-received client message)
  (let ([content (string-trim (rc:message-content message))]
        [channel (rc:message-channel-id message)])
    (if (and (not (message-from-bot? message))
             (string-prefix? content prefix))
        (let ([response (dispatch-command client message (strip-trim (rc:message-content message) prefix))])
          (http:create-message client channel response))
        #f)))

(define (init-client token)
  (let ([client (rc:make-client token #:auto-shard #t)])
    (rc:on-event 'message-create client message-received)
    client))

(define (main)
  (let ([token (getenv "BOT_TOKEN")])
    (if (not token)
        (error "No token provided")
        (rc:start-client (init-client token)))))

(module* main #f
  (main))
