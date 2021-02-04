#!/usr/bin/env racket
#lang racket

(require (prefix-in shlex: shlex))
(require (prefix-in rc: racket-cord))
(require (prefix-in sz: racket/serialize))
(require (prefix-in http: racket-cord/http))
(require (prefix-in ev: "evaluator.rkt"))
(require (prefix-in db: "trick-db.rkt"))

; williewillus#8490, maintainer, & Vazkii#0999, whose server we hope to run this in
(define bot-admins '("132691314784337920" "156785583723642881"))

(define prefix "!rkt ")

(define (strip-trim msg prefix)
  (string-trim (substring msg (string-length prefix))))

(define (message-from-bot? message)
  (and (not (null? (rc:message-author message)))
       (not (null? (rc:user-bot (rc:message-author message))))))

(define ((contextualizer client) message)
  (let ([channel (http:get-channel client (rc:message-channel-id message))])
    (and (rc:guild-channel? channel) (rc:guild-channel-guild-id channel))))

(define (make-db client filename) (db:make-trickdb (contextualizer client) filename))

(define message-author-id (compose1 rc:user-id rc:message-author))

(sz:serializable-struct trick (author body created))

(define (can-modify? trick message)
  (let ([author-id (message-author-id message)])
    (or
      (equal? (trick-author trick) author-id)
      ; TODO: Put a check here for Administrator/Manage Members:
      ; Use HTTP to turn message to channel to guild obj, search author ID to get member
      ; Then iter owned roles and lookup in guild to bit-check perms
      (member author-id bot-admins))))

(define (strip-backticks code)
  (let ([groups (regexp-match #px"```(\\w+\n)?(.+)```" code)])
    (if groups
        (caddr groups)
        code)))

(define (codeblock-quote result)
  (~a "```scheme\n" result "```"))

(define (split-once str)
  (let ([index (index-where (string->list str) char-whitespace?)])
    (if index
      (values (substring str 0 index) (string-trim (substring str index)))
      (values str #f))))

(define-syntax-rule (check-trick-prereqs db message text context-out name-out body-out body)
  (let ([context-out (db:get-trick-context db message)])
    (if context-out
      (let-values ([(name-out body-out) (split-once text)])
        (if (non-empty-string? name-out)
          body
          (~a "Missing the name for the trick!")))
      (~a "Cannot run tricks for this type of message!"))))

(define (make-trick body message) (trick (message-author-id message) (strip-backticks body) (rc:message-timestamp message)))

(define (evaluation-ctx client message args)
  `((message-contents . ,(rc:message-content message))
    (args             . ,args)
    ; TODO: This doesn't get work, it gets blocked by HTTP sandbox; perhaps send a message to a worker thread?
    (delete-caller    . ,(thunk (http:delete-message client (rc:message-channel-id message) (rc:message-id message))))))

(define (run-snippet client _ message code)
  (let ([code (strip-backticks code)]
        [text (rc:message-content message)])
    (ev:run code (evaluation-ctx client message '()))))

(define (register-trick client db message text)
  (check-trick-prereqs
    db message text
    context name body
    (cond
      [(not body) (~a "Trick " name " needs a body!")]
      [(db:add-trick! context name (thunk (make-trick body message)))
        (~a "Successfully registered trick " name "!")]
      [else (~a "Trick " name " already exists!")])))

(define (call-trick client db message text)
  (check-trick-prereqs
    db message text
    context name body
    (let ([trick (db:get-trick context name)])
      (if trick
        (codeblock-quote
          (ev:run
            (trick-body trick)
            (evaluation-ctx
              client
              message
              (if body (shlex:split body) '()))))
        (~a "Trick " name " doesn't exist!")))))

(define (show-trick client db message text)
  (check-trick-prereqs
    db message text
    context name _
    (let ([trick (db:get-trick context name)])
      (if trick
        (~a
          "Source for trick **"
          name
          "**, created by "
          (get-user-tag (trick-author trick))
          ":\n"
          (codeblock-quote (trick-body trick)))
        (~a "Trick " name " doesn't exist!")))))

(define help
  (string-replace
    (string-join
     '("R16 -- A Racket Trick Bot for Discord"
       "Brought to you by williewillus, Alwinfy, and Eutro"
       ""
       "Commands:"
       "-  PREFIXeval <code> => evaluate the rest of the message as a Racket form"
       "-  PREFIXregister <name> <code> => register the given Racket form as a trick with the given name"
       "-  PREFIXcall <name> ... => invoke the named trick, passing it the rest of the message as arguments; arguments are split shell-style"
       "-  PREFIXshow <name> => show metadata and source for the named trick"
       "-  PREFIXupdate <name> => change the source of the named trick; requires ownership or administrator"
       "-  PREFIXdelete <name> => delete the named trick; requires ownership or administrator and cannot be undone!"
       "-  PREFIXhelp => show this message"
       ""
       "The following data is available in the trick environment:"
       "-  all symbols from the `threading-lib` package (for utility purposes)"
       "-  message-contents => Full text of the invoking command, as a string"
       "-  args => List of string arguments to the trick"
       "-  delete-self => Thunk that removes the message that invoked the trick")
     "\n")
    "PREFIX" prefix))

(define command-table
  `(("eval"     . ,run-snippet)
    ("register" . ,register-trick)
    ("call"     . ,call-trick)
    ("help"     . ,(thunk* help))
    ("show"     . ,show-trick)))

(define (dispatch-command client db message text)
  (ormap (lambda (pair)
           (match-let ([(cons cmdname func) pair])
             (and (string-prefix? text cmdname)
                  (func client db message (strip-trim text cmdname)))))
         command-table))

(define ((message-received db) client message)
  (let ([content (string-trim (rc:message-content message))]
        [channel (rc:message-channel-id message)])
    (if (and (not (message-from-bot? message))
             (string-prefix? content prefix))
        (let ([response (dispatch-command client db message (strip-trim (rc:message-content message) prefix))])
          (http:create-message client channel response))
        #f)))

(define (init-client token)
  (let* ([client (rc:make-client token #:auto-shard #t)]
         [db     (make-db client "tricks.rktd")])
    (thread (thunk (sleep 60) (db:commit-db! db)))
    (rc:on-event 'message-create client (message-received db))
    client))

(define (main)
  (let ([token (getenv "BOT_TOKEN")])
    (if (not token)
        (error "No token provided")
        (rc:start-client (init-client token)))))

(module* main #f
  (main))
