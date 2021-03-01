#lang racket

(require
  racket/contract
  racket/hash
  racket/serialize)

(define saveable-trick? serializable?)

; A context id specifies the environment tricks belong to. Most commonly, it is a
; guild id or DM channel id
(define context-id? string?)
(define trick-key? string?)
; A permission check is a way to ensure a trick can be modified or removed.
(define permission-check? (or/c (-> saveable-trick? boolean?) #f))

(provide
 trickdb?
  (contract-out
    (saveable-trick? (-> any/c boolean?))
    (make-trickdb (-> path-string? trickdb?))
    (list-tricks (-> trickdb? context-id? (listof trick-key?)))
    (all-tricks (-> trickdb? context-id? (listof (cons/c trick-key? saveable-trick?))))
    (get-trick (-> trickdb? context-id? trick-key? (or/c saveable-trick? #f)))
    (add-trick! (-> trickdb? context-id? trick-key? (-> saveable-trick?) boolean?))
    (update-trick! (-> trickdb? context-id? trick-key? (-> saveable-trick? saveable-trick?) permission-check? boolean?))
    (remove-trick! (-> trickdb? context-id? trick-key? permission-check? boolean?))
    (commit-db! (-> trickdb? boolean?))))

; data: context-id -> (trick-key -> trick)
(struct trickdb (data filename (dirty #:mutable) lock))

(define (serialize-db db)
  (serialize (hash->list trickdb-data db)))

(define (try-read-db filename default)
  (with-handlers ([exn:fail? (lambda (e) (displayln e (current-error-port)) (default))])
    (make-hash (deserialize (read (open-input-file filename))))))

(define (make-trickdb filename)
  (let ([data (try-read-db filename make-hash)])
    (trickdb
     data
     filename
     #f
     (make-semaphore 1))))

(define-syntax-rule (with-db-lock db . body)
  (call-with-semaphore (trickdb-lock db) (thunk . body)))

(define (mark-dirty db) (set-trickdb-dirty! db #t))

; Note: db lock must be held
(define (get-submap db context-id)
  (and context-id
       (hash-ref!
        (trickdb-data db)
        context-id
        (thunk (make-hash)))))

(define (list-tricks db context-id)
  (with-db-lock db
    (hash-keys (get-submap db context-id))))

(define (all-tricks db context-id)
  (with-db-lock db
    (hash->list (get-submap db context-id))))

(define (get-trick db context-id name)
  (with-db-lock db
    (hash-ref (get-submap db context-id) name #f)))

(define (add-trick! db context-id name thunk)
  (with-db-lock db
    (let* ((table  (get-submap db context-id))
           (create (not (hash-has-key? table name))))
      (when create
        (log-info (~a "Trick created: " name))
        (mark-dirty db)
        (hash-set! table name (thunk)))
      create)))

(define (update-trick! db context-id name thunk perm-check)
  (with-db-lock db
    (let* ((table  (get-submap db context-id))
           (modify (and (hash-has-key? table name) (perm-check (hash-ref table name)))))
      (when modify
        (log-info (~a "Trick updated: " name))
        (mark-dirty db)
        (hash-set! table name (thunk (hash-ref table name))))
      modify)))

(define (remove-trick! db context-id name perm-check)
  (with-db-lock db
    (let* ((table  (get-submap db context-id))
           (remove (and (hash-has-key? table name) (perm-check (hash-ref table name)))))
      (when remove
        (log-info (~a "Trick deleted: " name))
        (mark-dirty db)
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
              (log-debug "db flushed")
              #t))))))
