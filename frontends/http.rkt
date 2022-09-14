#lang racket/base

(require
 racket/class
 racket/list
 racket/match
 racket/function
 racket/string
 racket/include
 racket/port

 "../common.rkt"
 "../config.rkt"
 "../log.rkt"
 (prefix-in ev: "../evaluator.rkt")
 "../interface.rkt"

 xml
 net/base64
 file/sha1
 net/url-structs
 web-server/web-server
 web-server/servlet-dispatch
 web-server/templates
 web-server/http/request-structs
 web-server/http/response-structs

 (for-syntax racket/base
             racket/port
             syntax/location))

(provide r16-make-frontend)

(define-for-syntax hotload-templates
  (environment-variables-ref
   (current-environment-variables)
   #"R16_HOTLOAD_TEMPLATES"))

(define-syntax (syntax-relative-path stx)
  (syntax-case stx ()
    [(_ path)
     (quasisyntax/loc stx
       (build-path
        #,(syntax-source-directory stx)
        path))]))

(define-syntax (include-bytes stx)
  (syntax-case stx ()
    [(_ path)
     (if hotload-templates
         (quasisyntax/loc stx
           (with-input-from-file (syntax-relative-path path) port->bytes))
         (quasisyntax/loc stx
           (include/reader
            path
            (lambda (_ port)
              (if (port-closed? port)
                  eof
                  (datum->syntax
                   #'#,stx
                   (port->bytes port #:close? #t)))))))]))

(define-syntax (maybe-define-namespace-anchor stx)
  (syntax-case stx ()
    [(_ name)
     (if hotload-templates
         (quasisyntax/loc stx
           (define-namespace-anchor name))
         #'(begin))]))

(maybe-define-namespace-anchor anchor)

(define-syntax (include-template* stx)
  (syntax-case stx ()
    [(_ path args ...)
     (if hotload-templates
         (quasisyntax/loc stx
           (let ([namespace (namespace-anchor->namespace anchor)])
             (namespace-set-variable-value!
              'args args
              #f namespace) ...
             (eval #'(include-template path)
                   namespace)))
         (quasisyntax/loc stx
           (include-template path)))]))

(struct image (bytes))

(define (simple-response code msg)
  (define title msg)
  (define navbar? #t)
  (define body (include-template* "http/simple.html" msg))
  (response/full
   code #f
   (current-seconds) TEXT/HTML-MIME-TYPE
   null
   (list
    (string->bytes/utf-8
     (include-template* "http/base.html" title navbar? body)))))

(define (format-for-html str)
  (include-template* "http/raw.html" str))

(define (response/html #:title title #:navbar? [navbar? #f] . body)
  (response/full
   200 #f
   (current-seconds) TEXT/HTML-MIME-TYPE
   null
   (list (string->bytes/utf-8 (include-template* "http/base.html" title navbar? body)))))

(define http-frontend
  (class* object% [r16-frontend<%>]
    (init-field port)

    (define trick-modify-mutex (make-semaphore 1))
    (define current-password-hash (make-parameter #f))
    (define/public (response? v) (image? v))
    (define/public (get-enrich-context)
      (define (enrich-context base _trick _args _parent-ctx)
        (define (make-image png-bytes)
          (image png-bytes))
        `(((make-image . ,make-image)
           ,@(car base))
          ,@(cdr base)))
      enrich-context)
    (define/public (can-modify? trick)
      (equal? (trick-author trick) (current-password-hash)))
    (define/public (start)
      (log-r16-debug "Starting server on port ~a" port)
      (parameterize ([current-frontend this])
        (serve
         #:dispatch (dispatch/servlet handle-request)
         #:listen-ip "127.0.0.1"
         #:port port)
        (do-not-return)))

    (define (handle-request req)
        (define req-uri (request-uri req))
        (define path (takef (url-path req-uri) (compose1 non-empty-string? path/param-path)))
        (match* [(request-method req) path]
          [[#"POST" (list (path/param "tricks" _)
                          (path/param "submit" _))]
           (define bindings (request-bindings/raw req))
           (match* [(bindings-assq #"name" bindings)
                    (bindings-assq #"code" bindings)
                    (bindings-assq #"password" bindings)]
             [[(binding:form _ (and name-bytes
                                    (pregexp "^[a-zA-Z_\\-0-9]+$")
                                    (app bytes->string/utf-8 name)))
               (binding:form _ (app bytes->string/utf-8 code))
               (binding:form _ password)]
              (call-with-semaphore
               ;; protect against concurrent modification of the body
               trick-modify-mutex
               (thunk
                ;; hash the password, using the sha256sum of the trick name as salt;
                ;; this is not completely secure because the salt *should*
                ;; be generated randomly so as not to be predictable,
                ;; but the risk factor is estimated to be low;
                ;; this hash is then used as the "author" field of the trick
                ;; and checked against for modification perms
                (define salt (sha256-bytes name-bytes))
                (define hashed-pass (bytes->hex-string (sha256-bytes (bytes-append salt password))))
                (parameterize ([current-password-hash hashed-pass])
                  (cond
                    [(zero? (string-length code))
                     (match (send (current-backend) delete name)
                       [(cons 'ok msg) (simple-response 200 (string->bytes/utf-8 msg))]
                       [(list* 'err msg _) (simple-response 400 (string->bytes/utf-8 msg))])]
                    [else
                     (match (send (current-backend) register name code hashed-pass (current-seconds))
                       [(cons 'ok _)
                        (response/full
                         303 #"See Other"
                         (current-seconds) TEXT/HTML-MIME-TYPE
                         (list (make-header #"Location" (string->bytes/utf-8 (format "/tricks/~a.rkt" name))))
                         (list #"Redirecting..."))]
                       [(list* 'err msg _)
                        (simple-response 400 (string->bytes/utf-8 msg))])]))))]
             [[_ _ _]
              (simple-response 400 #"400 Bad Request")])]

          [[#"GET" (list (path/param "tricks" _))]
           (response/html
            #:title "Tricks"
            #:navbar? #t
            (include-template* "http/tricks.html"))]

          [[#"GET" (list (path/param "tricks" _)
                         (path/param (pregexp "^([a-zA-Z_\\-0-9]+)\\.rkt$" (list _ trick-name)) _))]
           (define trick-v (send (current-backend) lookup trick-name))
           (response/full
            200 #"OK"
            (current-seconds) #"text/plain; charset=utf-8"
            null
            (list (string->bytes/utf-8 (trick-body trick-v))))]

          [[#"GET" (list (path/param "tricks" _)
                         (path/param (pregexp "^[a-zA-Z_\\-0-9]+$" (list trick-name)) _))]
           (define args (assoc 'args (url-query req-uri)))
           (define resp (send (current-backend) call trick-name (if args (cdr args) "")))
           (match resp
             [(cons 'ok res)
              (define stderr (ev:run-result-stderr res))
              (define stdout (ev:run-result-stdout res))
              (define results (ev:run-result-results res))
              (match* [stderr stdout results]
                [[#f "" (list (image raw-bytes))]
                 (response/full
                  200 #f
                  (current-seconds) #"image/png"
                  null
                  (list raw-bytes))]
                [[_ _ _]
                 (response/full
                  200 #f
                  (current-seconds) TEXT/HTML-MIME-TYPE
                  null
                  (list (string->bytes/utf-8
                         (include-template*
                          "http/result.html"
                          trick-name
                          stderr
                          stdout
                          results))))])]
             [(list 'err msg 'no-such-trick)
              (simple-response 404 (string->bytes/utf-8 msg))]
             [(list* 'err msg _)
              (simple-response 400 (string->bytes/utf-8 msg))])]

          [[#"GET" (list (path/param (or "style.css" "error.css") _))]
           (response/full
            200 #f
            (current-seconds) #"text/css"
            null
            (list (include-bytes "http/style.css")))]

          [[#"GET" (list (path/param "favicon.ico" _))]
           (response/full
            200 #f
            (current-seconds) #"image/x-icon"
            null
            (list (include-bytes "http/favicon.ico")))]

          [[#"GET" (list (path/param "icon.svg" _))]
           (response/full
            200 #f
            (current-seconds) #"image/svg+xml"
            null
            (list (include-bytes "http/icon.svg")))]

          [[#"GET" (list)]
           (response/html
            #:title "r16"
            #:navbar? #t
            (include-template* "http/home.html"))]

          [[#"GET" _]
           (simple-response 404 #"404 Not Found")]))

    (super-new)))

(define (r16-make-frontend raw-config)
  (define port (check-config exact-positive-integer? (hash-ref raw-config 'port 8080)))
  (new http-frontend
       [port port]))
