#!/usr/bin/env racket
#lang racket

(require
  (prefix-in rc: racket-cord)
  (prefix-in http: racket-cord/http)
  (prefix-in db: "trick-db.rkt")

  (only-in shlex (split shlex:split))
  (only-in racket/serialize serializable-struct)
  (only-in "evaluator.rkt" (run ev:run)))

; williewillus#8490, maintainer, & Vazkii#0999, whose server we hope to run this in
(define bot-admins '("132691314784337920" "156785583723642881"))

(define prefix "!rkt ")
(define trick-prefix "!!")

(define (strip-trim msg prefix)
  (string-trim (substring msg (string-length prefix))))

(define (message-from-bot? message)
  (and (not (null? (rc:message-author message)))
       (not (null? (rc:user-bot (rc:message-author message))))))

(define user-cache (make-hash))
(define (get-user-tag client uid)
  (hash-ref! user-cache uid
    (thunk
      (let ([user (http:get-user client uid)])
        (format "~a#~a" (rc:user-username uid) (rc:user-discriminator uid))))))

(define ((contextualizer client) message)
  (let ([channel (http:get-channel client (rc:message-channel-id message))])
    (and (rc:guild-channel? channel) (rc:guild-channel-guild-id channel))))

(define (make-db client filename) (db:make-trickdb (contextualizer client) filename))

(define message-author-id (compose1 rc:user-id rc:message-author))

(serializable-struct trick (author body created))

(define (can-modify? message trick)
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

(define deleter-thread
  (thread
   (thunk (let loop ()
            (match-let ([(cons client message) (thread-receive)])
              (with-handlers ([exn:fail:network? identity])
                (http:delete-message client (rc:message-channel-id message) (rc:message-id message)))
              (loop))))))


(define (make-trick body message)
  (trick (message-author-id message)
         (strip-backticks body)
         (rc:message-timestamp message)))

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
              (or body ""))))
        (~a "Trick " name " doesn't exist!")))))

(define (update-trick client db message text)
  (check-trick-prereqs
    db message text
    context name body
    (cond
      [(not body) (~a "Trick " name " needs a body!")]
      [(db:update-trick! context name (thunk (make-trick body message)) (curry can-modify? message))
       (~a "Successfully updated trick " name "!")]
      [else (~a "Trick " name " doesn't exist, or you can't modify it!")])))

(define (delete-trick client db message text)
  (check-trick-prereqs
    db message text
    context name body
    (if (db:remove-trick! context name (thunk (make-trick body message)) (curry can-modify? message))
      (~a "Successfully removed trick " name "!")
      (~a "Trick " name " doesn't exist, or you can't remove it!"))))

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
       "-  string-args => Message contents after the bot command, as a string"
       "-  shlex-args => Message contents after the bot command, as split by the shlex package, or #f if there was a split failure"
       #;"-  delete-caller => Thunk that removes the message that invoked the trick")
     "\n")
    "PREFIX" prefix))

(define (evaluation-ctx client message args)
  (let ([shlex-args (thunk
                     (with-handlers ([exn:fail:read:eof? #f])
                       (shlex:split args)))])
    `((message-contents . ,(rc:message-content message))
      (string-args      . ,args)
      (shlex-args       . ,shlex-args)
      (delete-caller    . ,(thunk (thread-send deleter-thread (cons client message)))))))

(define command-table
  `(("eval"     . ,run-snippet)
    ("register" . ,register-trick)
    ("call"     . ,call-trick)
    ("update"   . ,update-trick)
    ("delete"   . ,delete-trick)
    ("help"     . ,(thunk* help))
    ("show"     . ,show-trick)))

(define (parse-command content)
  (cond
    ; If trick-prefix, return (call-trick rest)
    [(string-prefix? content trick-prefix)
     (cons call-trick (strip-trim content trick-prefix))]
    ; If prefix, find the command or fall through
    [(and
       (string-prefix? content prefix)
       (let ([content (strip-trim content prefix)])
         (ormap
           (lambda (pair)
             (match-let ([(cons cmdname func) pair])
               (and (string-prefix? content cmdname)
                    (cons func (strip-trim content cmdname)))))
           command-table)))]
    ; Return falsey value for func
    [else (cons #f content)]))

(define ((message-received db) client message)
  (let ([content (string-trim (rc:message-content message))]
        [channel (rc:message-channel-id message)])
    (unless (message-from-bot? message)
      (match-let ([(cons func content) (parse-command content)])
        (when func
          (let ([response (func client db message content)])
            (http:create-message client channel response)))))))

(define (init-client token)
  (let* ([client (rc:make-client token #:auto-shard #t)]
         [db     (make-db client "tricks.rktd")])
    (thread (thunk (sleep 30) (db:commit-db! db)))
    (rc:on-event 'message-create client (message-received db))
    client))

(define (main)
  (let ([token (getenv "BOT_TOKEN")])
    (if (not token)
        (error "No token provided")
        (rc:start-client (init-client token)))))

(module* main #f
  (main))
