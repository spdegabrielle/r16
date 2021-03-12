#lang racket

(require
 json
 racket/contract
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
    (make-trickdb (->* (path-string?) (path-string? (-> jsexpr? any/c)) trickdb?))
    (list-tricks (-> trickdb? context-id? (listof trick-key?)))
    (all-tricks (-> trickdb? context-id? (listof (cons/c trick-key? saveable-trick?))))
    (get-trick (-> trickdb? context-id? trick-key? (or/c saveable-trick? #f)))
    (add-trick! (-> trickdb? context-id? trick-key? (-> saveable-trick?) boolean?))
    (update-trick! (-> trickdb? context-id? trick-key? (-> saveable-trick? saveable-trick?) permission-check? boolean?))
    (remove-trick! (-> trickdb? context-id? trick-key? permission-check? boolean?))
    (commit-db! (->* (trickdb?) ((-> any/c jsexpr?) path-string?) boolean?))))

; data: context-id -> (trick-key -> trick)
(struct trickdb (data filename (dirty #:mutable) lock))

(define (serialize-db db)
  (serialize (hash->list (trickdb-data db))))

(define (try-read-db port)
  (make-hash (deserialize (read port))))

(define (load-data new-data-path json->trick)
  (define (load-tricks path dest)
    (define context-id (string-trim path ".json" #:left #f))
    (call-with-input-file* path
      (lambda (port)
        (define js (read-json port))
        (unless (hash? js)
          (error "Data file was not a json object"))
        (hash-set! dest context-id
                   (make-hash (hash-map (lambda (name trick) (cons name (json->trick trick))))))
        (log-info "Loaded ~a tricks from ~a" (hash-count (hash-ref dest context-id)) path))))
  (define (load-guild path acc)
    (when (string-suffix? path ".json")
      (load-tricks path acc))
    acc)
  (foldl load-guild (make-hash) (directory-list new-data-path)))

(define (load-data-legacy path)
  (with-handlers ([exn:fail? (lambda (e) (log-error (~a e)) (make-hash))])
    (call-with-input-file* path try-read-db)))

(define (make-trickdb filename [new-data-path #f] [json->trick #f])
  (let ([data (if (and new-data-path json->trick)
                  (with-handlers ([exn:fail?
                                   (lambda (e)
                                     (log-warning "Falling back to legacy data load because: ~a" e)
                                     (load-data-legacy filename))])
                    (load-data new-data-path json->trick))
                  (load-data-legacy filename))])
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

(define (save-new-output data trick->json folder)
  (log-debug "Saving new format data in ~a" folder)
  (with-handlers ([exn:fail:filesystem:exists? void])
    (make-directory folder))
  (hash-for-each
   data
   (lambda (guild submap)
     (define tricks-serialized
       (make-immutable-hash (hash-map submap (lambda (name trick)
                                               (cons (string->symbol name) (trick->json trick))))))
     (call-with-atomic-output-file
      (build-path folder (~a guild ".json"))
      (lambda (port _)
        (write-json tricks-serialized port)
        (log-debug "new format data written"))))))

(define (commit-db! db [trick->json #f] [folder #f])
  (with-db-lock db
    (and (trickdb-dirty db)
         (with-handlers ((exn:fail? (lambda (e)
                                      (log-error (~a "Error saving tricks: " e)) #f)))
           (when (and trick->json folder)
             (save-new-output (trickdb-data db) trick->json folder))
           (call-with-atomic-output-file
            (trickdb-filename db)
            (lambda (port _)
              (write (serialize-db db) port)
              (set-trickdb-dirty! db #f)
              (log-debug "db flushed")
              #t))))))

(module* test #f
  (require rackunit)

  (serializable-struct
   fake-trick
   (value)
   #:mutable
   #:transparent)

  (define (fake-trick->json ft)
    (hasheq 'value (fake-trick-value ft)))

  (define (json->fake-trick js)
    (fake-trick (hash-ref js 'value)))

  (current-logger (make-logger #f #f)) ; suppress stderr output so tests pass on pkgs.racket-lang.org

  (test-case "CRUD Smoke test"
    (define context-id "guild1")
    (define trick-id "trick1")
    (define db (make-trickdb "/tmp/dummy")) ; we aren't reading/writing this db so this doesn't matter
    (check-equal? (list-tricks db context-id) null)
    (check-equal? (all-tricks db context-id) null)

    (check-true (add-trick! db context-id trick-id (thunk (fake-trick "foo"))))
    (check-equal? (fake-trick-value (get-trick db context-id trick-id)) "foo")

    (check-equal? (length (list-tricks db context-id)) 1)
    (check-equal? (length (all-tricks db context-id)) 1)

    (check-true (update-trick! db context-id trick-id
                               (const (fake-trick "bar"))
                               (const #t)))
    (check-equal? (fake-trick-value (get-trick db context-id trick-id)) "bar")

    (check-true (remove-trick! db context-id trick-id (const #t)))
    (check-equal? (list-tricks db context-id) null)
    (check-equal? (all-tricks db context-id) null))

  (test-case "Duplicate and Missing Test"
    (define context-id "guild1")
    (define trick-id "trick1")
    (define db (make-trickdb "/tmp/dummy")) ; we aren't reading/writing this db so this doesn't matter

    (check-true (add-trick! db context-id trick-id (thunk (fake-trick "foo"))))
    (check-false (add-trick! db context-id trick-id (thunk (fake-trick "foo2"))))

    (check-false (update-trick! db context-id "nonexistent-id" values (const #t))
    (check-false (remove-trick! db context-id "nonexistent-id" (const #t)))))

  (test-case "Context Separation Test"
    (define context-id-1 "guild1")
    (define context-id-2 "guild2")
    (define trick-id "trick1")
    (define db (make-trickdb "/tmp/dummy")) ; we aren't reading/writing this db so this doesn't matter

    (check-true (add-trick! db context-id-1 trick-id (thunk (fake-trick "foo"))))
    (check-true (add-trick! db context-id-2 trick-id (thunk (fake-trick "bar"))))

    (check-equal? (fake-trick-value (get-trick db context-id-1 trick-id)) "foo")
    (check-equal? (fake-trick-value (get-trick db context-id-2 trick-id)) "bar"))

  (test-case "Saving Test (old format)"
    (define context-id-1 "guild1")
    (define context-id-2 "guild2")
    (define trick-id "trick1")
    (define path (make-temporary-file))
    (after
     (let ([db (make-trickdb path)])
       (check-true (add-trick! db context-id-1 trick-id (thunk (fake-trick "foo"))))
       (check-true (add-trick! db context-id-2 trick-id (thunk (fake-trick "bar"))))
       (check-true (commit-db! db)))
     (let ([db (make-trickdb path)])
       (check-equal? (fake-trick-value (get-trick db context-id-1 trick-id)) "foo")
       (check-equal? (fake-trick-value (get-trick db context-id-2 trick-id)) "bar"))

     (delete-file path)))

  (test-case "Saving Test (new format)"
    (define context-id-1 "guild1")
    (define context-id-2 "guild2")
    (define trick-id "trick1")
    (define legacy-file (make-temporary-file))
    (define path (make-temporary-file "rkttmp~a" 'directory))
    (after
     (let ([db (make-trickdb legacy-file path json->fake-trick)])
       (check-true (add-trick! db context-id-1 trick-id (thunk (fake-trick "foo"))))
       (check-true (add-trick! db context-id-2 trick-id (thunk (fake-trick "bar"))))
       (check-true (commit-db! db fake-trick->json path)))
     (let ([db (make-trickdb legacy-file path json->fake-trick)])
       (check-equal? (fake-trick-value (get-trick db context-id-1 trick-id)) "foo")
       (check-equal? (fake-trick-value (get-trick db context-id-2 trick-id)) "bar"))

     (begin
       (delete-file legacy-file)
       (delete-directory/files path)))))
