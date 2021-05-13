#!/usr/bin/env racket
#lang racket

(require
 (prefix-in rc: racket-cord)
 (prefix-in http: racket-cord/http)
 (prefix-in db: "trick-db.rkt")

 (only-in "evaluator.rkt" (run ev:run))
 "log.rkt"
 (only-in net/url get-pure-port string->url)
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

(define (context-id message)
  (or (rc:message-guild-id message) (rc:message-channel-id message)))

(define message-author-id (compose1 rc:user-id rc:message-author))

(struct trick
  (author
   body
   created
   [storage #:mutable]
   [invocations #:mutable]))

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

(define-syntax-rule (check-trick-prereqs message text context-out name-out body-out body)
  (let ([context-out (context-id message)])
    (let-values ([(name-out body-out) (split-once text)])
      (if (non-empty-string? name-out)
          body
          (~a "Missing the name for the trick!")))))

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
         (if parent (trick-storage parent) (make-hash))
         (if parent (trick-invocations parent) 0)))

(define (run-snippet client db message code)
  (let ([code (strip-backticks code)])
    (with-typing-indicator client message
      (thunk (ev:run code (evaluation-ctx #f client message db (context-id message) "" #f) http:attachment?)))))

(define (register-trick client db message text)
  (check-trick-prereqs
   message text
   context-id name body
   (cond
     [(not body) (~a "Trick " name " needs a body!")]
     [(db:add-trick! db context-id name (thunk (make-trick body message #f)))
      (~a "Successfully registered trick " name "!")]
     [else (~a "Trick " name " already exists!")])))

(define (call-trick client db message text)
  (check-trick-prereqs
   message text
   context-id name body
   (let ([trick (db:get-trick db context-id name)])
     (if trick
         (begin
           (db:update-trick! db context-id name
                             (lambda (t) (set-trick-invocations! t (add1 (trick-invocations t))) t)
                             (const #t))
           (with-typing-indicator client message
             (thunk (ev:run
                     (trick-body trick)
                     (evaluation-ctx
                      trick
                      client
                      message
                      db
                      context-id
                      (or body "")
                      #f)
                     http:attachment?))))
         (~a "Trick " name " doesn't exist!")))))

(define (update-trick client db message text)
  (check-trick-prereqs
   message text
   context-id name body
   (cond
     [(not body) (~a "Trick " name " needs a body!")]
     [(db:update-trick! db context-id name (curry make-trick body message) (curry can-modify? message))
      (~a "Successfully updated trick " name "!")]
     [else (~a "Trick " name " doesn't exist, or you can't modify it!")])))

(define (delete-trick client db message text)
  (check-trick-prereqs
   message text
   context-id name _
   (if (db:remove-trick! db context-id name (curry can-modify? message))
       (~a "Successfully removed trick " name "!")
       (~a "Trick " name " doesn't exist, or you can't remove it!"))))

(define (uptime)
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

(define (stats client db message text)
  (~a "Uptime (dd:hh:mm:ss): " (uptime)
      "\n"
      "Bytes in use: " (current-memory-use)))

(define (cmp-tricks lt rt)
  (let ([l (cdr lt)] [r (cdr rt)])
    (if (= (trick-invocations l) (trick-invocations r))
        (string>? (trick-created l) (trick-created r))
        (> (trick-invocations l) (trick-invocations r)))))

(define (popular-tricks client db message text)
  (let ([tricks (sort (db:all-tricks db (context-id message)) cmp-tricks)])
    (if (empty? tricks)
        (~a "There aren't any tricks registered in your guild! Use `" prefix "register` to create one.")
        (apply ~a "**Most popular tricks in your guild:**"
               (for/list ([(trick i)
                           (in-indexed
                            (if (> (length tricks) leaderboard-size)
                                (take tricks leaderboard-size)
                                tricks))])
                 (~a
                  "\n" (add1 i) ". **" (car trick) "**, by <@" (trick-author (cdr trick))
                  ">, invoked **" (trick-invocations (cdr trick)) "**x"))))))

(define (show-trick client db message text)
  (check-trick-prereqs
   message text
   context-id name _
   (let ([trick (db:get-trick db context-id name)])
     (if trick
         (~a
          "Trick **"
          name
          "**, created by <@"
          (trick-author trick)
          ">, has been invoked **`"
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
      "PREFIX**eval** [_code_]:  evaluate [_code_] as a Racket form"
      "PREFIX**call** [_name_] ...:  invoke the trick [_name_], evaluating its source code in a fresh sandbox"
      "!![_trickname_]:  shorthand for !rkt call [_trickname_]"
      ""
      "PREFIX**register** [_name_] [_code_]:  register [_code_] as a trick with name [_name_]"
      "PREFIX**show** [_name_]:  show metadata and source for the trick [_name_]"
      "PREFIX**update** [_name_] [_code_]:  change the source of the trick [_name_]; requires ownership or administrator"
      "PREFIX**delete** [_name_]:  delete the trick [_name_]; requires ownership or administrator and cannot be undone!"
      ""
      "PREFIX**popular**:  show a leaderboard of popular tricks"
      "PREFIX**about**:  show version info"
      "PREFIX**help**:  show this message"
      "PREFIX**stats**:  show operational stats"
      ""
      "For documentation on what is available in the trick environment, please see the R16 documentation:"
      "https://docs.racket-lang.org/r16/index.html")
    "\n")
   "PREFIX" prefix))

(define/contract (make-attachment data name type)
  (-> bytes? (or/c string? bytes?) (or/c symbol? string? bytes?) http:attachment?)
  (http:attachment data (~a type) name))
(define/contract ((call-subtrick client db context-id message parent-ctx) name arguments)
  (-> rc:client? db:trickdb? string? rc:message? any/c (-> (or/c symbol? string?) any/c any))
  (let ([trick (db:get-trick db context-id (~a name))])
    (if trick
        (match-let
            ([(list stdout vals ... stderr)
              (call-with-values
               (thunk (ev:run
                       (trick-body trick)
                       (evaluation-ctx
                        trick
                        client
                        message
                        db
                        context-id
                        (if arguments (~a arguments) "")
                        parent-ctx)))
               list)])
          (write-string stdout)
          (unless (void? stderr) (write-string stderr (current-error-port)))
          (apply values vals))
        (raise (make-exn:fail:contract (~a "Trick " name " doesn't exist!"))))))

(define (storage-info message type)
  (match type
    ['guild   (cons 65536 'global)]
    ['channel (cons 8192  (rc:message-channel-id message))]
    ['user    (cons 2048  (message-author-id message))]
    [_        (cons 0     #f)]))

(define/contract (read-storage trick message type)
  (-> (or/c trick? #f) rc:message? (or/c 'guild 'channel 'user) any/c)
  (let ([datum (and~> trick
                      trick-storage
                      (hash-ref (cdr (storage-info message type)) #f)
                      (with-input-from-string read)
                      (with-handlers ([exn:fail:read? (const #f)]) _))])
    (and (not (eof-object? datum)) datum)))
(define/contract (write-storage trick message type data)
  (-> (or/c trick? #f) rc:message? (or/c 'guild 'channel 'user) any/c boolean?)
  (and
   trick
   (match-let ([(cons limit key) (storage-info message type)])
     (and
      key
      (let ([data (with-output-to-string (curry write data))])
        (and
         (<= (string-length data) limit)
         (begin
           (hash-set! (trick-storage trick) key data)
           #t)))))))

; client -> (emote name -> emote id)
(define emote-lookup-cache (make-hash))

; client -> set of emote ids known by the bot
(define emote-whitelist-cache (make-hash))

; emote id -> bytes
(define emote-image-cache (make-hash))
(define emote-image-thread
  (thread
   (thunk
    (let loop ()
      (let ([message (thread-receive)])
        ; TODO this only uses PNG, racket-cord needs to expose an animated field on emoji
        (channel-put (cdr message)
                     (with-handlers ([exn:fail? (const #f)])
                       (~> (~a "https://cdn.discordapp.com/emojis/" (car message) ".png?v=1")
                           string->url
                           get-pure-port
                           port->bytes))))
      (loop)))))
(define/contract ((emote-image client) id)
  (-> rc:client? (-> string? (or/c bytes? #f)))
  (hash-ref!
   emote-image-cache id
   (thunk
    (and 
     ; Is this an emote that this bot has encountered?
     ; If not, don't bother requesting it and just return #f
     (set-member? (hash-ref! emote-whitelist-cache client
                             ; COFU a set of all emotes in the lookup table
                             (thunk (~> emote-lookup-cache
                                        (hash-ref client)
                                        hash-values
                                        list->set)))
                  id)
     (let ([ch (make-channel)])
       (thread-send emote-image-thread (cons id ch))
       (let ([data (channel-get ch)])
         ; If empty byte string returned, return #f
         (and data (positive? (bytes-length data)) data)))))))

(define (evaluation-ctx trick client message db context-id args parent-ctx)
  (let* ([placeholder (make-placeholder #f)]
         [ctx
          `((message-contents . ,(rc:message-content message))
            (string-args      . ,args)
            (read-args        . ,(thunk
                                  (with-handlers ([exn:fail:read? #f])
                                    (let loop ([data (open-input-string args)])
                                      (let ([val (read data)])
                                        (if (eof-object? val) null (cons val (loop data))))))))
            (emote-lookup     . ,(curry hash-ref
                                        (hash-ref! emote-lookup-cache client
                                                   (thunk (for*/hash ([(_ guild) (rc:client-guilds client)]
                                                                      [emoji     (rc:guild-emojis guild)])
                                                            (values (rc:emoji-name emoji) (rc:emoji-id emoji)))))))
            (emote-image      . ,(emote-image client))
            (delete-caller    . ,(thunk (thread-send deleter-thread (cons client message))))
            (make-attachment  . ,make-attachment)
            (call-trick       . ,(call-subtrick client db context-id message placeholder))
            (message-author   . ,(message-author-id message))
            (read-storage     . ,(curry read-storage trick message))
            (write-storage    . ,(curry write-storage trick message))
            (parent-context   . ,parent-ctx))])
    (placeholder-set! placeholder (make-hash ctx))
    (cons (make-reader-graph ctx) '(threading))))

(define (trick->json trick)
  (hasheq 'author (trick-author trick)
          'body (trick-body trick)
          'created (trick-created trick)
          'data (trick-storage trick)
          'invocations (trick-invocations trick)))

(define (json->trick json)
  (trick
   (hash-ref json 'author)
   (hash-ref json 'body)
   (hash-ref json 'created)
   (hash-ref json 'data make-hash)
   (hash-ref json 'invocations)))

(define command-table
  `(("about"    . ,(const about))
    ("call"     . ,call-trick)
    ("delete"   . ,delete-trick)
    ("eval"     . ,run-snippet)
    ("help"     . ,(const help))
    ("popular"  . ,popular-tricks)
    ("register" . ,register-trick)
    ("save"     . ,(lambda (client db msg text) (if (db:commit-db! db trick->json) "Saved" "Nothing to save or error saving")))
    ("show"     . ,show-trick)
    ("update"   . ,update-trick)
    ("stats"   . ,stats)))

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

(define ((create-message-with-contents client channel message) . contents)
  (let* ([content (apply ~a #:separator "\n" (filter string? contents))]
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
                            (create-message-with-contents client channel message)))))))

(define (init-client folder token)
  (log-r16-info "Storing tricks in ~a" folder)
  (let* ([client (rc:make-client token
                                 #:auto-shard #t
                                 #:intents (list rc:intent-guilds rc:intent-guild-messages))]
         [db     (db:make-trickdb folder json->trick)])
    (thread
     (thunk
      (let loop ()
        (sleep 30)
        (db:commit-db! db trick->json)
        (loop))))
    (rc:on-event 'message-create client (message-received db))
    client))

(define (get-folder)
  (define argv (current-command-line-arguments))
  (if (< (vector-length argv) 1)
      (raise-user-error "Please pass the directory to be used to store trick data.")
      (vector-ref argv 0)))

(define (main)
  (define discord-receiver (make-log-receiver rc:discord-logger 'debug))
  (define r16-receiver (make-log-receiver r16-logger 'debug))
  (thread
   (thunk
    (let loop ()
      (let ([v (sync discord-receiver r16-receiver)])
        (printf "[~a] ~a\n"
                (vector-ref v 0)
                (vector-ref v 1)))
      (loop))))
  (set! start-time (current-seconds))
  (rc:start-client (init-client (get-folder)
                                (getenv "BOT_TOKEN"))))

(module* main #f
  (main))
