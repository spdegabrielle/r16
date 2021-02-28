#!/usr/bin/env racket
#lang racket

(require
  (prefix-in rc: racket-cord)
  (prefix-in http: racket-cord/http)
  (prefix-in db: "trick-db.rkt")

  (only-in racket/serialize serializable-struct/versions)
  (only-in "evaluator.rkt" (run ev:run))
  threading)

(define prefix "!rkt ")
(define trick-prefix "!!")
(define leaderboard-size 10)
(define start-time 0)

(define (strip-trim msg prefix)
  (string-trim (substring msg (string-length prefix))))

(define (message-from-bot? message)
  (and (rc:message-author message)
       (rc:user-bot (rc:message-author message))))

(define user-cache (make-hash))
(define (get-user-tag client uid)
  (hash-ref! user-cache uid
    (thunk
      (let ([user (http:get-user client uid)])
        (format "~a#~a" (rc:user-username user) (rc:user-discriminator user))))))

(define ((contextualizer client) message)
  (let ([channel (http:get-channel client (rc:message-channel-id message))])
    (and (rc:guild-channel? channel) (rc:guild-channel-guild-id channel))))

(define (make-db client filename) (db:make-trickdb (contextualizer client) filename))

(define message-author-id (compose1 rc:user-id rc:message-author))

(serializable-struct/versions trick 1 (author body created (invocations #:mutable))
  ([0
    (curryr trick 0)
    #f]))

(define (can-modify? message trick)
  (let ([author-id (message-author-id message)]
        [perms (bitwise-ior rc:permission-administrator
                            rc:permission-manage-guild)])
    (or
      (equal? (trick-author trick) author-id)
      (let ([memb (rc:message-member message)])
        (and~> memb
               rc:guild-member-permissions
               string->number
               (bitwise-and perms)
               ((negate zero?)))))))

(define (strip-backticks code)
  (let ([groups (regexp-match #px"```(\\w+\n)?(.+)```|`(.+)`" code)])
    (if groups
        (or (third groups) (fourth groups))
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

(define typing-thread
  (thread
   (thunk
    (let loop ([data #hash()])
      (match-let ([(list val client channel) (thread-receive)])
        (let* ([key (cons (rc:user-id (rc:client-user client)) channel)]
               [newval (+ val (hash-ref data key 0))])
          (unless (zero? newval)
            (with-handlers ([exn:fail? (const #f)]) (http:trigger-typing-indicator client channel)))
          (loop (hash-set data key newval))))))))

(define (with-typing-indicator client message thunk)
  (let ([payload (list client (rc:message-channel-id message))])
    (thread-send typing-thread (cons 1 payload))
    (let ([result (call-with-values thunk list)])
      (thread-send typing-thread (cons -1 payload))
      (apply values result))))


(define (make-trick body message parent)
  (trick (if parent (trick-author parent) (message-author-id message))
         (strip-backticks body)
         (if parent (trick-created parent) (rc:message-timestamp message))
         (if parent (trick-invocations parent) 0)))

(define (run-snippet client db message code)
  (let ([code (strip-backticks code)]
        [text (rc:message-content message)])
    (with-typing-indicator client message
      (thunk (ev:run code (evaluation-ctx client message (db:get-trick-context db message) "" #f))))))

(define (register-trick client db message text)
  (check-trick-prereqs
    db message text
    context name body
    (cond
      [(not body) (~a "Trick " name " needs a body!")]
      [(db:add-trick! context name (thunk (make-trick body message #f)))
       (~a "Successfully registered trick " name "!")]
      [else (~a "Trick " name " already exists!")])))

(define (call-trick client db message text)
  (check-trick-prereqs
    db message text
    context name body
    (let ([trick (db:get-trick context name)])
      (if trick
        (begin
          (db:update-trick! context name (lambda (t) (set-trick-invocations! t (add1 (trick-invocations t))) t) (const #t))
          (with-typing-indicator client message
            (thunk (ev:run
              (trick-body trick)
              (evaluation-ctx
                client
                message
                context
                (or body "")
                #f)))))
        (~a "Trick " name " doesn't exist!")))))

(define (update-trick client db message text)
  (check-trick-prereqs
    db message text
    context name body
    (cond
      [(not body) (~a "Trick " name " needs a body!")]
      [(db:update-trick! context name (curry make-trick body message) (curry can-modify? message))
       (~a "Successfully updated trick " name "!")]
      [else (~a "Trick " name " doesn't exist, or you can't modify it!")])))

(define (delete-trick client db message text)
  (check-trick-prereqs
    db message text
    context name body
    (if (db:remove-trick! context name (curry can-modify? message))
      (~a "Successfully removed trick " name "!")
      (~a "Trick " name " doesn't exist, or you can't remove it!"))))

(define (uptime client db message text)
  (define seconds-in-minute 60)
  (define seconds-in-hour (* 60 60))
  (define seconds-in-day (* 24 60 60))
  (let*-values ([(v) (- (current-seconds) start-time)]
                [(days v) (quotient/remainder v seconds-in-day)]
                [(hours v) (quotient/remainder v seconds-in-hour)]
                [(minutes seconds) (quotient/remainder v seconds-in-minute)])
    (~>> (list days hours minutes seconds)
         (map (lambda (x) (~a #:min-width 2 #:align 'right #:pad-string "0" x)))
         (string-join _ ":"))))

(define (cmp-tricks lt rt)
  (let ([l (cdr lt)] [r (cdr rt)])
    (if (= (trick-invocations l) (trick-invocations r))
      (string>? (trick-created l) (trick-created r))
      (> (trick-invocations l) (trick-invocations r)))))

(define (popular-tricks client db message text)
  (let ([tricks (sort (db:all-tricks (db:get-trick-context db message)) cmp-tricks)])
    (if (empty? tricks)
      (~a "There aren't any tricks registered in your guild! Use `" prefix "register` to create one.")
      (apply ~a "**Most popular tricks in your guild:**"
        (for/list ([(trick i)
                    (in-indexed
                     (if (> (length tricks) leaderboard-size)
                       (take tricks leaderboard-size)
                       tricks))])
          (~a
            "\n" (add1 i) ". **" (car trick) "**, by " (get-user-tag client (trick-author (cdr trick)))
            ", invoked **" (trick-invocations (cdr trick)) "**x"))))))

(define (show-trick client db message text)
  (check-trick-prereqs
    db message text
    context name _
    (let ([trick (db:get-trick context name)])
      (if trick
        (~a
          "Trick **"
          name
          "**, created by "
          (get-user-tag client (trick-author trick))
          ", has been invoked **`"
          (trick-invocations trick)
          "`** times.\n__Source code:__\n"
          (codeblock-quote (trick-body trick)))
        (~a "Trick " name " doesn't exist!")))))

(define about
  (string-join
   `("R16 -- A Racket Trick Bot for Discord"
     ,(~a "Running on Racket " (version))
     "Brought to you by williewillus, Alwinfy, and Eutro"
     "Project Homepage: https://sr.ht/~williewillus/r16")
   "\n"))

(define help
  (string-replace
    (string-join
     `("Commands:"
       "-  PREFIXeval <code> => evaluate <code> as a Racket form"
       "-  PREFIXregister <name> <code> => register <code> as a trick with name <name>"
       "-  PREFIXcall <name> ... => invoke the named trick, evaluating its source code verbatim in a fresh sandbox"
       "-  !!<trickname> => shorthand for PREFIXcall <trickname>"
       "-  PREFIXshow <name> => show metadata and source for the named trick"
       "-  PREFIXupdate <name> <code> => change the source of the named trick; requires ownership or administrator"
       "-  PREFIXdelete <name> => delete the named trick; requires ownership or administrator and cannot be undone!"
       "-  PREFIXpopular => show a leaderboard of popular tricks"
       "-  PREFIXhelp => show this message"
       ""
       "The following data is available in the trick environment:"
       "-  all symbols from the `threading-lib` package (for utility purposes)"
       "-  make-attachment => Procedure that makes an attachment from a byte-string, file name and MIME type"
       "-  call-trick => Procedure that calls another trick given a name and arguments"
       "-  message-contents => Full text of the invoking command, as a string"
       "-  string-args => Message contents after the bot command, as a string"
       "-  read-args => Thunk that returns message contents after the bot command, as split by the Racket reader, or #f if there was a split failure"
       "-  delete-caller => Thunk that removes the call or eval command that ran this code"
       "-  parent-context => Mapping of the above symbols for the trick calling this one, or #f if this trick is top-level")
     "\n")
    "PREFIX" prefix))

(define/contract (make-attachment data name type)
  (-> bytes? (or/c string? bytes?) (or/c symbol? string? bytes?) http:attachment?)
  (http:attachment data (~a type) name))
(define/contract ((call-subtrick client trick-ctx message parent-ctx) name arguments)
  (-> rc:client? db:trick-context? rc:message? any/c (-> (or/c symbol? string?) any/c any))
  (let ([trick (db:get-trick trick-ctx (~a name))])
    (if trick
      (match-let
        ([(list stdout vals ... stderr)
          (call-with-values
            (thunk (ev:run
                    (trick-body trick)
                    (evaluation-ctx
                      client
                      message
                      trick-ctx
                      (if arguments (~a arguments) "")
                      parent-ctx)))
            list)])
        (write-string stdout)
        (unless (void? stderr) (write-string stderr (current-error-port)))
        (apply values vals))
      (raise (make-exn:fail:contract (~a "Trick " name " doesn't exist!"))))))

(define (evaluation-ctx client message trick-ctx args parent-ctx)
  (let* ([placeholder (make-placeholder #f)]
         [ctx
          `((message-contents . ,(rc:message-content message))
            (string-args      . ,args)
            (read-args        . ,(thunk
                                  (with-handlers ([exn:fail:read? #f])
                                    (let loop ([data (open-input-string args)])
                                      (let ([val (read data)])
                                        (if (eof-object? val) null (cons val (loop data))))))))
            (delete-caller    . ,(thunk (thread-send deleter-thread (cons client message))))
            (make-attachment  . ,make-attachment)
            (call-trick       . ,(call-subtrick client trick-ctx message placeholder))
            (parent-context   . ,parent-ctx))])
    (placeholder-set! placeholder (make-hash ctx))
    (cons (make-reader-graph ctx) '(threading))))


(define command-table
  `(("about"    . ,(const about))
    ("call"     . ,call-trick)
    ("delete"   . ,delete-trick)
    ("eval"     . ,run-snippet)
    ("help"     . ,(const help))
    ("popular"  . ,popular-tricks)
    ("register" . ,register-trick)
    ("save"     . ,(lambda (client db msg text) (if (db:commit-db! db) "Saved" "Nothing to save or error saving")))
    ("show"     . ,show-trick)
    ("update"   . ,update-trick)
    ("uptime"   . ,uptime)))

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

(define char-cap 2000)
(define slice-size 30)

(define (truncate-string str cap)
  (let ([len (string-length str)])
    (if (> len cap)
      (let* ([slicepos (- cap slice-size)] [restsize (- len slicepos)])
        (format "~a... [~a more characters]" (substring str 0 slicepos) restsize))
      str)))

(define (empty-string? s)
  (and (string? s) (= (string-length s) 0)))

(define (create-message-with-contents client channel message . contents)
  (let* ([content (apply ~a #:separator "\n"
                         (filter-not (disjoin void? http:attachment? empty-string?)
                                     contents))]
         [attachment (findf http:attachment? contents)]
         [content (if (or attachment (non-empty-string? content))
                      (truncate-string content char-cap)
                      "\u200b")]
         [reference (hash 'message_id (rc:message-id message)
                          'guild_id (rc:message-guild-id message))])
    (http:create-message client channel content
                         #:file attachment
                         #:reply-to reference
                         #:allowed-mentions (hash 'parse '()))))

(define ((message-received db) client message)
  (let ([content (string-trim (rc:message-content message))]
        [channel (rc:message-channel-id message)])
    (unless (message-from-bot? message)
      (match-let ([(cons func content) (parse-command content)])
        (when func
          (call-with-values (thunk (func client db message content))
                            (curry create-message-with-contents client channel message)))))))

(define (init-client token)
  (let* ([client (rc:make-client token
                                 #:auto-shard #t
                                 #:intents (list rc:intent-guilds rc:intent-guild-messages))]
         [db     (make-db client "tricks.rktd")])
    (thread
      (thunk
        (let loop ()
          (sleep 30)
          (db:commit-db! db)
          (loop))))
    (rc:on-event 'message-create client (message-received db))
    client))

(define (get-token)
  (let* ([port (open-input-file "token")]
         [token (read-line port)])
    (close-input-port port)
    token))

(define dr (make-log-receiver rc:discord-logger 'debug))

(define (main)
  (thread
   (thunk
    (let loop ()
      (let ([v (sync dr)])
        (printf "[~a] ~a\n" (vector-ref v 0)
                (vector-ref v 1)))
      (loop))))
  (set! start-time (current-seconds))
  (rc:start-client (init-client (get-token))))

(module* main #f
  (main))
