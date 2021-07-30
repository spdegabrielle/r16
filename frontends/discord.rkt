#lang racket

(require "../interface.rkt"
         "../log.rkt"
         "../common.rkt"
         "../config.rkt"
         (prefix-in rc: racket-cord)
         (prefix-in http: racket-cord/http)
         (prefix-in ev: "../evaluator.rkt")
         (only-in net/url get-pure-port string->url)
         threading)

(provide r16-make-frontend)

(define (message-from-bot? message)
  (and (hash-ref message 'author #f)
       (hash-ref (hash-ref message 'author) 'bot #f)))

(define (context-id message)
  (or (hash-ref message 'guild_id #f) (hash-ref message 'channel_id #f)))

(define (message-author-id message)
  (hash-ref (hash-ref message 'author) 'id))

(define discord-frontend%
  (class* object% [r16-frontend<%>]
    (init-field client)
    (init-field bot-prefix)
    (init-field trick-prefix)

    (define typing-thread
      (thread
       (thunk
        (let loop ([data #hash()])
          (match-let ([(list val client channel) (thread-receive)])
            (let* ([key (cons (hash-ref (rc:client-user client) 'id) channel)]
                   [newval (+ val (hash-ref data key 0))])
              (unless (zero? newval)
                (with-handlers ([exn:fail? (const #f)]) (http:trigger-typing-indicator client channel)))
              (loop (hash-set data key newval))))))))

    (define deleter-thread
      (thread
       (thunk
        (let loop ()
          (match-let ([(cons client message) (thread-receive)])
            (with-handlers ([exn:fail:network? identity])
              (http:delete-message
               client
               (hash-ref message 'channel_id)
               (hash-ref message 'id)))
            (loop))))))

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

    (define current-message (make-parameter #f))

    (define (format-run-result rr)
      `(,(ev:run-result-stdout rr)
        ,@(ev:run-result-results rr)
        ,@(let ([stderr (ev:run-result-stderr rr)])
            (if stderr
                (list (string-append "\n:warning: stderr:\n" stderr))
                null))))

    (define/public (response? v)
      (http:attachment? v))

    (define/public (can-modify? trick-obj)
      (define author-id (message-author-id (current-message)))
      (define perms (bitwise-ior rc:permission-administrator
                                 rc:permission-manage-guild))
      (or
       (equal? (trick-author trick-obj) author-id)
       (and~> (current-message)
              (hash-ref _ 'member)
              (hash-ref _ 'permissions)
              string->number
              (bitwise-and perms)
              ((negate zero?)))))

    ; emote name -> emote id
    (define emote-lookup-cache (make-hash))

    ; set of emote ids known by the bot
    (define known-emotes (mutable-set))

    ; emote id -> bytes
    (define emote-image-cache (make-hash))

    (define/public (get-enrich-context)
      (define message (current-message))
      (define message-contents (hash-ref message 'content))
      (define message-author (message-author-id message))

      (define/contract (emote-image id)
        (-> string? (or/c bytes? #f))
        (hash-ref!
         emote-image-cache
         id
         (thunk
          (and 
           ; Is this an emote that this bot has encountered?
           ; If not, don't bother requesting it and just return #f
           (set-member? known-emotes id)
           (let ([ch (make-channel)])
             (thread-send emote-image-thread (cons id ch))
             (let ([data (channel-get ch)])
               ; If empty byte string returned, return #f
               (and data (positive? (bytes-length data)) data)))))))

      (define/contract (make-attachment data name type)
        (-> bytes? (or/c string? bytes?) (or/c symbol? string? bytes?) http:attachment?)
        (http:attachment data (~a type) name))

      (define (storage-info type)
        (match type
          ['guild   (cons 65536 'global)]
          ['channel (cons 8192  (string->symbol (hash-ref message 'channel_id)))]
          ['user    (cons 2048  (string->symbol (message-author-id message)))]
          [_        (cons 0     #f)]))

      (define/contract ((read-storage trick) type)
        (-> (or/c trick? #f) (-> (or/c 'guild 'channel 'user) any/c))
        (let ([datum (and~> trick
                            trick-storage
                            (hash-ref (cdr (storage-info type)) #f)
                            (with-input-from-string read)
                            (with-handlers ([exn:fail:read? (const #f)]) _))])
          (and (not (eof-object? datum)) datum)))
      (define/contract ((write-storage trick) type data)
        (-> (or/c trick? #f) (-> (or/c 'guild 'channel 'user) any/c boolean?))
        (and
         trick
         (match-let ([(cons limit key) (storage-info type)])
           (and
            key
            (let ([data (with-output-to-string (curry write data))])
              (and
               (<= (string-length data) limit)
               (begin
                 (hash-set! (trick-storage trick) key data)
                 #t)))))))

      (define (delete-caller)
        (thread-send deleter-thread (cons client message)))

      (lambda (base trick-obj _args _parent-context)
        `(((message-contents . ,message-contents)
           (message-author   . ,message-author)
           (emote-lookup     . ,(curry hash-ref emote-lookup-cache))
           (emote-image      . ,emote-image)
           (delete-caller    . ,delete-caller)
           (make-attachment  . ,make-attachment)
           (read-storage     . ,(read-storage trick-obj))
           (write-storage    . ,(write-storage trick-obj))
           (attachment-data  . ,http:attachment-data)
           ,@(car base))
          ,@(cdr base))))

    (define/public (start)
      (define discord-receiver (make-log-receiver rc:discord-logger 'debug))
      (thread
       (thunk
        (let loop ()
          (let ([v (sync discord-receiver)])
            (printf "[~a] ~a\n"
                    (vector-ref v 0)
                    (vector-ref v 1)))
          (loop))))
      (~>
       (let loop ()
        (sleep 30)
        (define result (send (current-backend) save))
        (when (exn:fail? result)
          (log-r16-error (~a "Error saving tricks: " result)))
        (loop))
       thunk thread)
      (rc:on-event 'raw-message-create client message-received)
      (rc:on-event 'raw-guild-create client guild-create)
      (rc:start-client client))

    (define (guild-create _ws-client _client guild)
      ; eagerly fill all the emote mappings for each guild, so we don't need to touch the
      ; network when tricks call emote-id
      (let ([known (mutable-set)])
        (for ([emote (in-list (hash-ref guild 'emojis null))])
          (hash-set! emote-lookup-cache (hash-ref emote 'name) (hash-ref emote 'id))
          (set-add! known (hash-ref emote 'id)))
        (set-union! known-emotes known)
        (log-r16-debug "Preloaded ~a emote ID's" (set-count known))))

    (define (message-received _ws-client _client message)
      (parameterize ([current-message message]
                     [current-context-id (context-id message)])
        (define content (string-trim (hash-ref message 'content)))
        (define channel (hash-ref message 'channel_id))
        (unless (message-from-bot? message)
          (match-let ([(cons func content) (parse-command content)])
            (when func
              (create-message-with-contents
               channel
               message
               (with-handlers
                 ([exn?
                   (lambda (e)
                     (define port (open-output-string))
                     (parameterize ([current-error-port port])
                       ((error-display-handler) (exn-message e) e))
                     (define error-message (get-output-string port))
                     (log-r16-error (~a "Internal error:\n" error-message))
                     (list (~a ":warning: Internal error:\n" error-message)))])
                 (func content))))))))

    (define (create-message-with-contents channel message contents)
      (define char-cap 2000)
      (define slice-size 30)

      (define (truncate-string str cap)
        (define len (string-length str))
        (if (> len cap)
            (let* ([slicepos (- cap slice-size)] [restsize (- len slicepos)])
              (format "~a... [~a more characters]" (substring str 0 slicepos) restsize))
            str))

      (define raw-content (string-trim (apply ~a #:separator "\n" (filter string? contents))))
      (define attachment (findf http:attachment? contents))
      (define content
        (if (or attachment (non-empty-string? raw-content))
            (truncate-string raw-content char-cap)
            "\u200b"))
      (define reference (hash 'message_id (hash-ref message 'id)))

      (http:create-message client channel content
                           #:file attachment
                           #:reply-to reference
                           #:allowed-mentions (hash 'parse '())))

    (define (with-typing-indicator thunk)
      (let ([payload (list client (hash-ref (current-message) 'channel_id))])
        (thread-send typing-thread (cons 1 payload))
        (let ([result (call-with-values thunk list)])
          (thread-send typing-thread (cons -1 payload))
          (apply values result))))

    (define parse-command
      (let ()
        (define (split-once str)
          (let ([index (index-where (string->list str) char-whitespace?)])
            (if index
                (values (substring str 0 index) (string-trim (substring str index)))
                (values str ""))))

        (define-syntax-rule (check-trick-prereqs
                             [(name-out body-out) text]
                             body ...)
          (let-values ([(name-out body-out) (split-once text)])
            (cond
              [(non-empty-string? name-out)
               body ...]
              [else
               (list (~a "Missing the name for the trick!"))])))

        (struct command (func help)
          #:property prop:procedure (struct-field-index func))
        (define-syntax-rule (define/command (name text)
                              help
                              body ...)
          (define name (command (lambda (text) body ...) help)))

        (define-syntax-rule (define/command/trick (name name-binding
                                                        body-binding)
                              help
                              body ...)
          (define/command (name text)
            help
            (check-trick-prereqs
             [(name-binding body-binding) text]
             body ...)))

        (define (strip-backticks code)
          (let ([groups (regexp-match #px"```(\\w+\n)?(.+)```|`(.+)`" code)])
            (if groups
                (or (third groups) (fourth groups))
                code)))

        (define (codeblock-quote result)
          (~a "```scheme\n" result "```"))

        (define/command (call-snippet text)
          " [_code_]:  evaluate [_code_] as a Racket form"
          (with-typing-indicator
            (thunk
             (define result
               (send (current-backend) evaluate (strip-backticks text)))
             (if (ev:run-result? result)
                 (format-run-result result)
                 (list result)))))

        (define/command/trick (call-trick name body)
          " [_name_] ...:  invoke the trick [_name_], evaluating its source code in a fresh sandbox"
          (with-typing-indicator
            (thunk
             (define result
               (send (current-backend) call name body))
             (if (ev:run-result? result)
                 (format-run-result result)
                 (list result)))))

        (define/command/trick (register-trick name body)
          " [_name_] [_code_]:  register [_code_] as a trick with name [_name_]"
          (list
           (send (current-backend) register
                 name (strip-backticks body)
                 (message-author-id (current-message))
                 (hash-ref (current-message) 'timestamp))))

        (define/command/trick (show-trick name _body)
          " [_name_]:  show metadata and source for the trick [_name_]"
          (define trick-obj (send (current-backend) lookup name))
          (list
           (if trick-obj
               (format
                (~a
                 #:separator "\n"
                 "Trick **~a**, created by <@~a>, has been invoked `~a` times."
                 "__Source code:__"
                 "~a")
                name
                (trick-author trick-obj)
                (trick-invocations trick-obj)
                (codeblock-quote (trick-body trick-obj)))
               (~a "Trick " name " doesn't exist!"))))

        (define/command/trick (update-trick name body)
          " [_name_] [_code_]:  change the source of the trick [_name_]; requires ownership or administrator"
          (list (send (current-backend) update name (strip-backticks body))))

        (define/command/trick (delete-trick name _body)
          " [_name_]:  delete the trick [_name_]; requires ownership or administrator and cannot be undone!"
          (list (send (current-backend) delete name)))

        (define/command (popular text)
          ":  show a leaderboard of popular tricks"
          (define leaderboard-size 10)
          (define tricks (send (current-backend) popular))
          (define pages (exact-ceiling (/ (length tricks) leaderboard-size)))
          (define pageno
            (~> text (string->number 10 'number-or-false) (or 1)
                inexact->exact (max 1) (min pages) sub1))
          (define page (drop tricks (* leaderboard-size pageno)))
          (list
           (if (empty? tricks)
               (~a "There aren't any tricks registered in your guild! \
                    Use `" bot-prefix "register` to create one.")
               (apply
                ~a "**Most popular tricks in your guild (page " (add1 pageno) " of " pages "):**"
                (for/list ([i (in-naturals (add1 (* leaderboard-size pageno)))]
                           [trick
                            (if (> (length page) leaderboard-size)
                                (take page leaderboard-size)
                                page)])
                  (~a
                   "\n" i ". **" (car trick) "**, by <@" (trick-author (cdr trick))
                   ">, invoked **" (trick-invocations (cdr trick)) "**x"))))))

        (define/command (about _text)
          ":  show version info"
          (list (send (current-backend) about)))

        (define/command (help _text)
          ":  show this message"
          (list help-message))

        (define/command (stats _text)
          ":  show operational stats"
          (list (send (current-backend) stats)))

        (define (save _text)
          (list
           (match (send (current-backend) save)
             ['success "Saved"]
             ['unchanged "Nothing to save"]
             [e (~a "Error saving tricks: " e)])))

        (define command-table
          (hash
           "eval" call-snippet
           "call" call-trick

           "register" register-trick
           "show" show-trick
           "update" update-trick
           "delete" delete-trick

           "popular" popular
           "about" about
           "help" help
           "stats" stats

           ;; hidden
           "save" save))

        (define (format-command-help name)
          (define command (hash-ref command-table name))
          (format "~a**~a**~a" bot-prefix name (command-help command)))

        (define help-message
          (~>
           `("Commands:"
             ,(format-command-help "eval")
             ,(format-command-help "call")
             ,(format "~a[_trickname_]: shorthand for ~acall [_trickname_]"
                      trick-prefix bot-prefix)
             ""
             ,(format-command-help "register")
             ,(format-command-help "show")
             ,(format-command-help "update")
             ,(format-command-help "delete")
             ""
             ,(format-command-help "popular")
             ,(format-command-help "about")
             ,(format-command-help "help")
             ,(format-command-help "stats")
             ""
             "For documentation on what is available in the trick environment, please see the R16 documentation:"
             "https://docs.racket-lang.org/r16/index.html")
           (string-join "\n")))

        (define (strip-trim msg prefix)
          (string-trim (substring msg (string-length prefix))))

        (define (parse-command content)
          (cond
            ; if trick-prefix, return (call-trick rest)
            [(string-prefix? content trick-prefix)
             (cons call-trick (strip-trim content trick-prefix))]
            ; if prefix, find the command or fall through
            [(and
              (string-prefix? content bot-prefix)
              (let ()
                (define-values (command args)
                  (split-once (strip-trim content bot-prefix)))
                (define found (hash-ref command-table command #f))
                (and found (cons found (string-trim args)))))]
            ; return falsey value for func
            [else (cons #f content)]))

        parse-command))

    (super-new)))

(define discord-config?
  (config/c
   [bot_token string?]))

(define (r16-make-frontend raw-config)
  (define config (check-config discord-config? raw-config))
  (define token (hash-ref config 'bot_token))
  (define bot-prefix (hash-ref config 'bot_prefix "!rkt "))
  (define trick-prefix (hash-ref config 'trick_prefix "!!"))
  (define client
    (rc:make-client
     token
     #:auto-shard #t
     #:intents (list rc:intent-guilds rc:intent-guild-messages)))
  (new discord-frontend%
       [client client]
       [bot-prefix bot-prefix]
       [trick-prefix trick-prefix]))
