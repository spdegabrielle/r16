#lang racket

(require
  (only-in racket-cord message?)
  racket/contract
  racket/hash
  racket/serialize)

(define saveable-trick? serializable?)

(define trick-key? string?)
; A contextualizer is a function mapping a message to a context key (e.g. guild ID)
(define contextualizer? (-> message? any))
; A permission check is a way to ensure a trick can be modified or removed.
(define permission-check? (or/c (-> saveable-trick? boolean?) #f))

(provide
  (contract-out
    (saveable-trick? (-> any/c boolean?))
    (make-trickdb (-> contextualizer? path-string? trickdb?))
    (get-trick-context (-> trickdb? message? (or/c trick-context? #f)))
    (list-tricks (-> trick-context? (listof trick-key?)))
    (get-trick (-> trick-context? trick-key? (or/c saveable-trick? #f)))
    (add-trick! (-> trick-context? trick-key? (-> saveable-trick?) boolean?))
    (update-trick! (-> trick-context? trick-key? (-> saveable-trick?) permission-check? boolean?))
    (remove-trick! (-> trick-context? trick-key? permission-check? boolean?))
    (commit-db! (-> trickdb? boolean?))))

; data: guild -> trick-context
(struct trickdb (data filename contextualizer (dirty #:mutable) lock))

; data: trick-key -> trick
(struct trick-context (data db))

(define (serialize-db db)
  (serialize
    (hash-map
      (trickdb-data db)
      ; Extract just the data portion of a context
      (lambda (k v) (cons k (trick-context-data v))))))

(define (deserialize-db db data)
  (make-hash
    (map
      (lambda (e) (cons (car e) (trick-context (cdr e) db)))
      (deserialize data))))

(define (try-read-db db filename default)
  (with-handlers ([exn:fail? (lambda (e) (displayln e (current-error-port)) (default))])
    (deserialize-db db (read (open-input-file filename)))))

(define (make-trickdb contextualizer filename)
  ; Due to the contexts having backreferences for locking, we instantiate the db first and then read in the ctxs
  (let ((db (trickdb
              (make-hash)
              filename
              contextualizer
              #f
              (make-semaphore 1))))
    (hash-union! (trickdb-data db) (try-read-db db filename make-hash))
    db))

(define-syntax-rule (with-db-lock db . body)
  (call-with-semaphore (trickdb-lock db) (thunk . body)))

(define (mark-dirty ctx) (set-trickdb-dirty! (trick-context-db ctx) #t))

(define (get-trick-context db message)
  (let ((id ((trickdb-contextualizer db) message)))
    (and id
      (with-db-lock db
        (hash-ref!
          (trickdb-data db)
          id
          (thunk
            (trick-context (make-hash) db)))))))

(define (list-tricks context)
  (with-db-lock (trick-context-db context)
    (hash-keys (trick-context-data context))))

(define (get-trick context name)
  (with-db-lock (trick-context-db context)
    (hash-ref (trick-context-data context) name #f)))

(define (add-trick! context name thunk)
  (with-db-lock (trick-context-db context)
    (let* ((table  (trick-context-data context))
           (create (not (hash-has-key? table name))))
      (when create
        (mark-dirty context)
        (hash-set! table name (thunk)))
      create)))

(define (update-trick! context name thunk perm-check)
  (with-db-lock (trick-context-db context)
    (let* ((table  (trick-context-data context))
           (modify (and (hash-has-key? table name) (perm-check (hash-ref table name)))))
      (when modify
        (mark-dirty context)
        (hash-set! table name (thunk)))
      modify)))

(define (remove-trick! context name perm-check)
  (with-db-lock (trick-context-db context)
    (let* ((table  (trick-context-data context))
           (remove (and (hash-has-key? table name) (perm-check (hash-ref table name)))))
      (when remove
        (mark-dirty context)
        (hash-remove! table name))
      remove)))

(define (commit-db! db)
  (with-db-lock db
    (and (trickdb-dirty db)
         (with-handlers ((exn:fail? (lambda (e)
                                      (log-error (~a "Error saving tricks: " e)) #f)))
           (call-with-atomic-output-file
            (trickdb-filename db)
            (lambda (port _)
              (write (serialize-db db) port)
              (set-trickdb-dirty! db #f)
              #t))))))
