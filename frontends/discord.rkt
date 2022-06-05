#lang racket/base

(require
 (only-in net/url get-pure-port string->url)
 racket/async-channel
 racket/class
 racket/contract
 (only-in racket/format ~a)
 (only-in racket/function const curry identity negate thunk)
 racket/list
 racket/match
 (only-in racket/math exact-ceiling)
 (only-in racket/port port->bytes with-input-from-string with-output-to-string)
 racket/set
 racket/string
 threading
 (prefix-in http: racket-cord/http)
 (prefix-in rc: racket-cord)
 "../common.rkt"
 "../config.rkt"
 (prefix-in ev: "../evaluator.rkt")
 "../interface.rkt"
 "../log.rkt"
 "../utils.rkt")

(provide r16-make-frontend)

(define (message-from-bot? message)
  (and (hash-ref message 'author #f)
       (hash-ref (hash-ref message 'author) 'bot #f)))

(define (context-id message)
  (or (hash-ref message 'guild_id #f) (hash-ref message 'channel_id #f)))

(define (message-author-id message)
  (hash-ref (hash-ref message 'author) 'id))

;; Defines a function that delegates to an external worker thread.
;; Used to expose limited IO capabilities to inside of a sandbox, as body
;; runs outside of the sandbox
(define-syntax-rule (define/off-thread (name args ...)
                      body ...)
  (begin
    (define worker-thread
      (thread-loop
       (match-define (vector chan args ...) (thread-receive))
       (define res (let () body ...))
       (async-channel-put chan res)))
    (define (name args ...)
      (define chan (make-async-channel 1))
      (thread-send worker-thread (vector chan args ...))
      (async-channel-get chan))))

(struct permission-manager
  (default-role-permission
    permissions-by-role
    guild-owner-id))

(define (make-permission-manager guild)
  (define-values (default-perms roles)
    (for/fold ([default-perms 0]
               [role-perms null])
              ([role (hash-ref guild 'roles)])
      (define perms (string->number (hash-ref role 'permissions)))
      (if (string=? (hash-ref role 'name) "@everyone")
          (values perms role-perms)
          (values default-perms (cons (cons (hash-ref role 'id) perms)
                                      role-perms)))))
  (permission-manager default-perms (make-hash roles) (hash-ref guild 'owner_id)))

(define (get-sender-permissions manager message)
  (define default-role-permission (permission-manager-default-role-permission manager))
  (define permissions-by-role (permission-manager-permissions-by-role manager))
  (define owner-id (permission-manager-guild-owner-id manager))
  (if (string=? owner-id (message-author-id message))
      -1
      (and~>> message
              (hash-ref _ 'member)
              (hash-ref _ 'roles)
              (map (λ (r) (hash-ref permissions-by-role r 0)))
              (foldl bitwise-ior default-role-permission))))

(define discord-frontend%
  (class* object% [r16-frontend<%>]
    (init-field client)
    (init-field bot-prefix)
    (init-field trick-prefix)

    (define with-typing-indicator ;; (_ proc)
      (let ()
        ;; channel -> active typing counter
        (define counters-box (box (make-immutable-hash)))

        (define (counters-swap! proc)
          (define old (unbox counters-box))
          (define newv (proc old))
          (if (box-cas! counters-box old newv)
              newv
              (counters-swap! proc)))

        (define (change-counter channel delta)
          (counters-swap!
           (lambda (counters)
             (~> (hash-ref counters channel 0)
                 (+ delta)
                 (match _
                   [0 (hash-remove counters channel)]
                   [v (hash-set counters channel v)]))))
          (maybe-trigger-typing channel))

        (define (maybe-trigger-typing channel)
          (when (hash-ref (unbox counters-box) channel #f)
            (trigger-typing channel)))

        (define (trigger-typing channel)
          (with-handlers ([exn:fail? void])
            (http:trigger-typing-indicator client channel)))

        (define _typing-thread
          (thread-loop
           (for ([(channel _) (unbox counters-box)])
             (trigger-typing channel))
           (sleep 5)))

        (lambda (proc)
          (define channel (hash-ref (current-message) 'channel_id))
          (dynamic-wind
            (thunk (change-counter channel 1))
            proc
            (thunk (change-counter channel -1))))))

    (define/off-thread (do-delete-message message)
      (with-handlers ([exn:fail:network? identity])
        (http:delete-message
         client
         (hash-ref message 'channel_id)
         (hash-ref message 'id))))

    (define/off-thread (get-emote-image id)
      (with-handlers ([exn:fail? (const #"")])
        ;; TODO this only uses PNG, racket-cord needs to expose an animated field on emoji
        (~> (~a "https://cdn.discordapp.com/emojis/" id ".png?v=1")
            string->url
            get-pure-port
            port->bytes)))

    (define/off-thread (open-attachment-url cust url)
      (with-handlers ([exn:fail? (const #f)])
        (parameterize ([current-custodian cust])
          (get-pure-port url))))

    (define current-message (make-parameter #f))
    (define current-deleted-box (make-parameter #f))

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
       (not (zero? (bitwise-and
                     perms
                     (get-perms-for (current-message)))))))

    ;; context/guild id -> (emote name -> emote id)
    (define emote-name-lookup (make-hash))

    ;; flattened set of all the leaf emote id's in emote-name-lookup, for fast contains
    (define known-emotes (box (set)))

    (define emote-image-cache
      (make-expiring-cache
       current-inexact-monotonic-milliseconds
       get-emote-image
       (* 10 60 1000))) ;; 10 min as ms
    (thread-loop
     (sleep 30)
     (define purged (length (expiring-cache-purge emote-image-cache)))
     (when (> purged 0)
       (log-r16-debug "Purged ~a emote image bytestrings" purged)))

    (define/public (get-enrich-context)
      (define deleted-box (current-deleted-box))
      (define message (current-message))
      (define message-contents (hash-ref message 'content))
      (define message-attachments (or (hash-ref message 'attachments #f) null))
      (define reply-message-attachments (or (and~> message (hash-ref 'referenced_message #f) (hash-ref 'attachments #f)) null))
      (define message-author (message-author-id message))
      (define channel-id (hash-ref message 'channel_id))

      (define/contract (emote-lookup name)
        (-> string? (or/c string? #f))
        (or
         ;; Check our own guild's emotes first
         (~> (context-id message)
             (hash-ref emote-name-lookup _ #hash())
             (hash-ref _ name #f))
         ;; Then try other guilds the bot is a member of (in arbitrary order)
         (for/or ([map (in-hash-values emote-name-lookup)])
           (hash-ref map name #f))))

      (define/contract (emote-image id)
        (-> string? (or/c bytes? #f))
        ;; Check if the emote is known to any of the guilds we're in.
        ;; This is to prevent tricks from abusing the bot to download unrelated emotes
        ;; from other servers it's not in.
        (and (set-member? (unbox known-emotes) id)
             (let ([data (expiring-cache-get emote-image-cache id)])
               (and (positive? (bytes-length data))
                    data))))

      (define/contract (make-attachment data name type)
        (-> bytes? (or/c string? bytes?) (or/c symbol? string? bytes?) http:attachment?)
        (http:attachment data (~a type) name))

      (define attachment-count (length message-attachments))
      (define reply-attachment-count (length reply-message-attachments))

      (define (fetch-attachment attachments index)
        (and (< index (length attachments))
             (open-attachment-url
              (current-custodian)
              (string->url (hash-ref (list-ref attachments index) 'url)))))

      (define/contract (open-attachment [index 0])
        (->* () (exact-nonnegative-integer?) (or/c input-port? #f))
        (fetch-attachment message-attachments index))

      (define/contract (open-reply-attachment [index 0])
        (->* () (exact-nonnegative-integer?) (or/c input-port? #f))
        (fetch-attachment reply-message-attachments index))

      (define (storage-info type)
        (match type
          ['guild   (cons 65536 'global)]
          ['channel (cons 8192  (string->symbol channel-id))]
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
        (when (box-cas! deleted-box #f #t)
          (do-delete-message message))
        (void))

      (define reply-contents
        (and~> message
               (hash-ref 'referenced_message #f)
               (hash-ref 'content #f)))

      (lambda (base trick-obj _args _parent-context)
        `(((message-contents       . ,message-contents)
           (message-author         . ,message-author)
           (emote-lookup           . ,emote-lookup)
           (emote-image            . ,emote-image)
           (delete-caller          . ,delete-caller)
           (make-attachment        . ,make-attachment)
           (read-storage           . ,(read-storage trick-obj))
           (write-storage          . ,(write-storage trick-obj))
           (attachment-data        . ,http:attachment-data)
           (open-attachment        . ,open-attachment)
           (open-reply-attachment  . ,open-reply-attachment)
           (attachment-count       . ,attachment-count)
           (reply-attachment-count . ,reply-attachment-count)
           (reply-contents         . ,reply-contents)
           ,@(car base))
          ,@(cdr base))))

    (define/public (start)
      (define discord-receiver (make-log-receiver rc:discord-logger 'warning))
      (thread
       (thunk
        (let loop ()
          (let ([v (sync discord-receiver)])
            (printf "[~a] ~a\n"
                    (vector-ref v 0)
                    (vector-ref v 1)))
          (loop))))
      (rc:on-event 'raw-message-create client message-received)
      (rc:on-event 'raw-guild-create client guild-create)
      (rc:on-event 'raw-guild-delete client guild-delete)
      (rc:on-event 'raw-guild-emojis-update client guild-emojis-update)
      (rc:start-client client))

    (define (extract-emojis data)
      (for/hash ([emote (in-list (hash-ref data 'emojis null))])
        (values (hash-ref emote 'name) (hash-ref emote 'id))))

    (define (recompute-known-emotes)
      (define new (for*/set ([name-to-id (in-hash-values emote-name-lookup)]
                             [id (in-hash-values name-to-id)])
                    id))
      (log-r16-debug "Know of ~a emotes" (set-count new))
      (set-box! known-emotes new))

    (define guild-perms-managers (make-hash))

    (define (get-perms-for message)
      (define manager (hash-ref guild-perms-managers (hash-ref message 'guild_id)))
      (get-sender-permissions manager message))

    (define (guild-create _ws-client _client guild)
      (hash-set! emote-name-lookup
                 (hash-ref guild 'id)
                 (extract-emojis guild))
      (hash-set! guild-perms-managers
                 (hash-ref guild 'id)
                 (make-permission-manager guild))
      (recompute-known-emotes))

    (define (guild-delete _ws-client _client guild)
      (hash-remove! emote-name-lookup (hash-ref guild 'id))
      (recompute-known-emotes))

    (define (guild-emojis-update _ws-client _client payload)
      (hash-set! emote-name-lookup
                 (hash-ref payload 'guild_id)
                 (extract-emojis payload))
      (recompute-known-emotes))

    (define (message-received _ws-client _client message)
      (parameterize ([current-frontend this]
                     [current-message message]
                     [current-deleted-box (box #f)]
                     [current-context-id (context-id message)])
        (define content (string-trim (hash-ref message 'content)))
        (define channel (hash-ref message 'channel_id))
        (unless (message-from-bot? message)
          (match-define (cons func func-args) (parse-command content))
          (when func
            (define contents
              (with-handlers
                ([exn?
                  (lambda (e)
                    (define port (open-output-string))
                    (parameterize ([current-error-port port])
                      ((error-display-handler) (exn-message e) e))
                    (define error-message (get-output-string port))
                    (log-r16-error (~a "Internal error:\n" error-message))
                    (list (~a ":warning: Internal error:\n" error-message)))])
                (func func-args)))
            (create-message-with-contents
             channel
             (and (not (unbox (current-deleted-box))) message)
             contents)))))

    (define (create-message-with-contents channel reply-message contents)
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
      (define reference
        (and reply-message
             (hash 'message_id (hash-ref reply-message 'id))))

      (http:create-message client channel content
                           #:file attachment
                           #:reply-to reference
                           #:allowed-mentions (hash 'parse '())))

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

        (define (error-response err)
          (list (car err)))

        (define/command (call-snippet text)
          " [_code_]:  evaluate [_code_] as a Racket form"
          (with-typing-indicator
            (thunk
             (define result
               (send (current-backend) evaluate (strip-backticks text)))
             (result-case format-run-result error-response result))))

        (define/command/trick (call-trick name body)
          " [_name_] ...:  invoke the trick [_name_], evaluating its source code in a fresh sandbox"
          (with-typing-indicator
            (thunk
             (define result
               (send (current-backend) call name body))
             (result-case format-run-result error-response result))))

        (define/command/trick (register-trick name body)
          " [_name_] [_code_]:  register [_code_] as a trick with name [_name_]"
          (define result
            (send (current-backend) register
                  name (strip-backticks body)
                  (message-author-id (current-message))
                  (hash-ref (current-message) 'timestamp)))
          (list (result-case cdr cadr result)))

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
          (list (result-case cdr cadr (send (current-backend) update name (strip-backticks body)))))

        (define/command/trick (delete-trick name _body)
          " [_name_]:  delete the trick [_name_]; requires ownership or administrator and cannot be undone!"
          (list (result-case cdr cadr (send (current-backend) delete name))))

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
           (if (null? tricks)
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
            ;; if trick-prefix, return (call-trick rest)
            [(string-prefix? content trick-prefix)
             (cons call-trick (strip-trim content trick-prefix))]
            ;; if prefix, find the command or fall through
            [(and
              (string-prefix? content bot-prefix)
              (let ()
                (define-values (command args)
                  (split-once (strip-trim content bot-prefix)))
                (define found (hash-ref command-table command #f))
                (and found (cons found (string-trim args)))))]
            ;; return falsey value for func
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
     #:intents (list rc:intent-guilds rc:intent-guild-messages rc:intent-guild-emojis)))
  (new discord-frontend%
       [client client]
       [bot-prefix bot-prefix]
       [trick-prefix trick-prefix]))
