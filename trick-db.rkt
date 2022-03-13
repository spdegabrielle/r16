#lang racket/base

(require
 json
 racket/contract
 (only-in racket/file call-with-atomic-output-file)
 (only-in racket/format ~a)
 (only-in racket/function const thunk)
 (only-in racket/list last)
 (only-in racket/string string-suffix? string-trim)
 (only-in racket/symbol symbol->immutable-string)
 "log.rkt")

;; A context id specifies the environment tricks belong to. Most commonly, it is a
;; guild id or DM channel id
(define context-id? string?)
(define trick-key? string?)
;; A permission check is a way to ensure a trick can be modified or removed.
(define permission-check? (or/c (-> any/c boolean?) #f))

(provide
 trickdb?
 (contract-out
  (make-trickdb (-> path-string? (-> jsexpr? any/c) trickdb?))
  (list-tricks (-> trickdb? context-id? (listof trick-key?)))
  (all-tricks (-> trickdb? context-id? (listof (cons/c trick-key? any/c))))
  (get-trick (-> trickdb? context-id? trick-key? (or/c any/c #f)))
  (add-trick! (-> trickdb? context-id? trick-key? (-> any/c) boolean?))
  (update-trick! (-> trickdb? context-id? trick-key? (-> any/c any/c) permission-check? boolean?))
  (remove-trick! (-> trickdb? context-id? trick-key? permission-check? boolean?))
  (commit-db! (-> trickdb? (-> any/c jsexpr?) boolean?))))

;; data: context-id -> (trick-key -> trick)
(struct trickdb (data filename (dirty #:mutable) lock))

(define (load-data dir json->trick)
  (define (load-tricks path dest)
    (define context-id (string-trim (path->string (last (explode-path path)))
                                    ".json"
                                    #:left? #f))
    (call-with-input-file* path
      (lambda (port)
        (define js (read-json port))
        (unless (hash? js)
          (error "Data file was not a json object"))
        (hash-set! dest context-id
                   (make-hash (hash-map js (lambda (name trick)
                                             (cons (symbol->immutable-string name)
                                                   (json->trick trick))))))
        (log-r16-info "Loaded ~a tricks from ~a" (hash-count (hash-ref dest context-id)) path))))
  (define (load-guild path acc)
    (let ([path (path->string path)])
      (when (string-suffix? path ".json")
        (load-tricks path acc)))
    acc)
  (foldl load-guild (make-hash) (directory-list dir #:build? #t)))

(define (make-trickdb path json->trick)
  (trickdb
   (load-data path json->trick)
   path
   #f
   (make-semaphore 1)))

(define-syntax-rule (with-db-lock db . body)
  (call-with-semaphore (trickdb-lock db) (thunk . body)))

(define (mark-dirty db) (set-trickdb-dirty! db #t))

;; Note: db lock must be held
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
        (log-r16-info (~a "Trick created: " name))
        (mark-dirty db)
        (hash-set! table name (thunk)))
      create)))

(define (update-trick! db context-id name thunk perm-check)
  (with-db-lock db
    (let* ((table  (get-submap db context-id))
           (modify (and (hash-has-key? table name) (perm-check (hash-ref table name)))))
      (when modify
        (log-r16-info (~a "Trick updated: " name))
        (mark-dirty db)
        (hash-set! table name (thunk (hash-ref table name))))
      modify)))

(define (remove-trick! db context-id name perm-check)
  (with-db-lock db
    (let* ((table  (get-submap db context-id))
           (remove (and (hash-has-key? table name) (perm-check (hash-ref table name)))))
      (when remove
        (log-r16-info (~a "Trick deleted: " name))
        (mark-dirty db)
        (hash-remove! table name))
      remove)))

(define (save data trick->json folder)
  (log-r16-debug "Saving data in ~a" folder)
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
        (write-json tricks-serialized port))))))

(define (commit-db! db trick->json)
  (with-db-lock db
    (and (trickdb-dirty db)
         (begin
           (save (trickdb-data db) trick->json (trickdb-filename db))
           (set-trickdb-dirty! db #f)
           #t))))

(module* test #f
  (require (only-in racket/file
                    delete-directory/files
                    make-temporary-file)
           rackunit)

  (struct
    fake-trick
    (value)
    #:mutable
    #:transparent)

  (define (fake-trick->json ft)
    (hasheq 'value (fake-trick-value ft)))

  (define (json->fake-trick js)
    (fake-trick (hash-ref js 'value)))

  (test-case "CRUD Smoke test"
    (define context-id "guild1")
    (define trick-id "trick1")
    (define path (make-temporary-file "rkttmp~a" 'directory))
    (define db (make-trickdb path json->fake-trick))

    (after 
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
     (check-equal? (all-tricks db context-id) null)

     (delete-directory/files path)))

  (test-case "Duplicate and Missing Test"
    (define context-id "guild1")
    (define trick-id "trick1")
    (define path (make-temporary-file "rkttmp~a" 'directory))
    (define db (make-trickdb path json->fake-trick))

    (after
     (check-true (add-trick! db context-id trick-id (thunk (fake-trick "foo"))))
     (check-false (add-trick! db context-id trick-id (thunk (fake-trick "foo2"))))

     (check-false (update-trick! db context-id "nonexistent-id" values (const #t))
                  (check-false (remove-trick! db context-id "nonexistent-id" (const #t))))

     (delete-directory/files path)))

  (test-case "Context Separation Test"
    (define context-id-1 "guild1")
    (define context-id-2 "guild2")
    (define trick-id "trick1")
    (define path (make-temporary-file "rkttmp~a" 'directory))
    (define db (make-trickdb path json->fake-trick))

    (after
     (check-true (add-trick! db context-id-1 trick-id (thunk (fake-trick "foo"))))
     (check-true (add-trick! db context-id-2 trick-id (thunk (fake-trick "bar"))))

     (check-equal? (fake-trick-value (get-trick db context-id-1 trick-id)) "foo")
     (check-equal? (fake-trick-value (get-trick db context-id-2 trick-id)) "bar")

     (delete-directory/files path)))

  (test-case "Saving Test"
    (define context-id-1 "guild1")
    (define context-id-2 "guild2")
    (define trick-id "trick1")
    (define path (make-temporary-file "rkttmp~a" 'directory))
    (after
     (let ([db (make-trickdb path json->fake-trick)])
       (check-true (add-trick! db context-id-1 trick-id (thunk (fake-trick "foo"))))
       (check-true (add-trick! db context-id-2 trick-id (thunk (fake-trick "bar"))))
       (check-true (commit-db! db fake-trick->json)))
     (let ([db (make-trickdb path json->fake-trick)])
       (check-equal? (fake-trick-value (get-trick db context-id-1 trick-id)) "foo")
       (check-equal? (fake-trick-value (get-trick db context-id-2 trick-id)) "bar"))
     (delete-directory/files path))))
