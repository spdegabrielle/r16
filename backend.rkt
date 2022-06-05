#lang racket/base

(require
 racket/class
 racket/contract
 (only-in racket/format ~a)
 (only-in racket/function const identity thunk)
 (only-in racket/sequence sequence->list)
 (only-in racket/string string-join)
 threading
 "common.rkt"
 (prefix-in ev: "evaluator.rkt")
 "interface.rkt"
 (prefix-in db: "trick-db.rkt"))

(provide r16%)

(define r16%
  (class* object% [r16-backend<%>]
    (init-field db)
    (field [start-time (current-seconds)])

    (define (evaluation-context enrich-context context-id trick name args parent-ctx)
      (define real-args (string->immutable-string args))
      (define this-context #f)

      (define/contract (call-subtrick name arguments)
        (-> (or/c symbol? string?) any/c any)
        (define strname (~a name))
        (define trick-obj (db:get-trick db context-id strname))
        (if trick-obj
            (let ()
              (define rr
                (ev:run
                 (trick-body trick-obj)
                 (evaluation-context
                  enrich-context
                  context-id
                  trick-obj
                  strname
                  (if arguments (~a arguments) "")
                  this-context)
                 (const #t)))
              (write-string (ev:run-result-stdout rr))
              (cond [(ev:run-result-stderr rr)
                     => (lambda (stderr) (write-string stderr (current-error-port)))])
              (apply values (ev:run-result-results rr)))
            (raise (make-exn:fail:contract (~a "Trick " name " doesn't exist!")))))

      (define (read-args)
        (with-handlers ([exn:fail:read? (const #f)])
          (sequence->list (in-producer read eof (open-input-string real-args)))))

      (define base
        `(((string-args    . ,real-args)
           (read-args      . ,read-args)
           (call-trick     . ,call-subtrick)
           (trick-name     . ,(string->immutable-string name))
           (parent-context . ,(and parent-ctx (make-immutable-hash (car parent-ctx)))))
          threading))
      (set! this-context (enrich-context base trick real-args parent-ctx))
      this-context)

    (define (response? x)
      (send (current-frontend) response? x))

    (define/public (evaluate code)
      (define enrich-context (send (current-frontend) get-enrich-context))
      (define ev-ctx (evaluation-context
                      enrich-context
                      (current-context-id)
                      #f "" "" #f))
      (cons 'ok (ev:run code ev-ctx response?)))

    (define/public (call name args)
      (define ctx-id (current-context-id))
      (define trick-obj (db:get-trick db ctx-id name))
      (cond
        [trick-obj
         (db:update-trick! db ctx-id name
                           (lambda (t)
                             (~> (trick-invocations t) add1
                                 (set-trick-invocations! t _))
                             t)
                           (const #t))
         (define enrich-context (send (current-frontend) get-enrich-context))
         (define ev-ctx (evaluation-context
                         enrich-context
                         (current-context-id)
                         trick-obj name args #f))
         (define code (trick-body trick-obj))
         (cons 'ok (ev:run code ev-ctx response?))]
        [else
         (list 'err (~a "Trick " name " doesn't exist!") 'no-such-trick)]))

    (define/public (delete name)
      (define ctx-id (current-context-id))
      (define trick-obj (db:get-trick db ctx-id name))
      (define frontend (current-frontend))
      (cond
        [(not trick-obj)
         (list 'err (~a "Trick " name " doesn't exist!") 'no-such-trick)]
        [(db:remove-trick!
          db ctx-id name
          (lambda (t) (send frontend can-modify? t)))
         (cons 'ok (~a "Successfully removed trick " name "!"))]
        [else
         (list 'err (~a "You cannot modify trick " name "!") 'missing-permissions)]))

    (define/public (register name code author timestamp)
      (cond
        [(zero? (string-length code))
         (list 'err (~a "Trick " name " needs a body!") 'needs-body)]
        [(db:add-trick!
          db (current-context-id) name
          (thunk (trick author code timestamp (make-hash) 0)))
         (cons 'ok (~a "Successfully registered trick " name "!"))]
        [else (update name code)]))

    (define/public (update name code)
      (define ctx-id (current-context-id))
      (define trick-obj (db:get-trick db ctx-id name))
      (define frontend (current-frontend))
      (cond
        [(not trick-obj)
         (list 'err (~a "Trick " name " doesn't exist!") 'no-such-trick)]
        [(zero? (string-length code))
         (list 'err (~a "Trick " name " needs a body!") 'needs-body)]
        [(db:update-trick!
          db ctx-id name
          (lambda (trick-obj)
            (trick (trick-author trick-obj)
                   code
                   (trick-created trick-obj)
                   (trick-storage trick-obj)
                   (trick-invocations trick-obj)))
          (lambda (t)
            (send frontend can-modify? t)))
         (cons 'ok (~a "Successfully updated trick " name "!"))]
        [else
         (list 'err (~a "You cannot modify trick " name "!") 'missing-permissions)]))

    (define/public (lookup name)
      (db:get-trick db (current-context-id) name))

    (define/public (popular)
      (sort (db:all-tricks db (current-context-id)) cmp-tricks))

    (define/public (save)
      (with-handlers ([exn:fail? identity])
        (if (db:commit-db! db trick->json)
            'success
            'unchanged)))

    (define/public (about)
      (string-join
       `("R16 -- A Racket Trick Bot"
         ,(~a "Running on Racket " (version))
         "Brought to you by williewillus, Alwinfy, and Eutro"
         "Project Homepage: https://sr.ht/~williewillus/r16")
       "\n"))

    (define/public (stats)
      (define all-tricks (db:all-tricks db (current-context-id)))
      (define total-invocations
        (for/sum ([pair (in-list all-tricks)])
          (trick-invocations (cdr pair))))
      (string-join
       (list (~a "Uptime (dd:hh:mm:ss): " (uptime))
             (~a "Bytes in use: " (current-memory-use))
             (~a "Total trick invocations (for your guild): " total-invocations))
       "\n"))

    (define (uptime)
      (define seconds-in-minute 60)
      (define seconds-in-hour (* 60 60))
      (define seconds-in-day (* 24 60 60))
      (let*-values
          ([(v) (- (current-seconds) start-time)]
           [(days v) (quotient/remainder v seconds-in-day)]
           [(hours v) (quotient/remainder v seconds-in-hour)]
           [(minutes seconds) (quotient/remainder v seconds-in-minute)])
        (~>> (list days hours minutes seconds)
             (map (lambda (x)
                    (~a #:min-width 2
                        #:align 'right
                        #:pad-string "0"
                        x)))
             (string-join _ ":"))))

    (super-new)))

(define (cmp-tricks lt rt)
  (let ([l (cdr lt)] [r (cdr rt)])
    (if (= (trick-invocations l) (trick-invocations r))
        (string>? (trick-created l) (trick-created r))
        (> (trick-invocations l) (trick-invocations r)))))
