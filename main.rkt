#!/usr/bin/env racket
#lang racket

(require json threading
         "common.rkt"
         "backend.rkt"
         "interface.rkt"
         "config.rkt"
         (prefix-in db: "trick-db.rkt"))

(define (readable? x)
  (and (string? x)
       (with-handlers ([void (const #f)])
         (read (open-input-string x))
         #t)))

(define r16-config?
  (config/c
   [frontend
    (or/c readable?
          (config/c
           [module readable?]))]
   [storage path-string?]))

(define help-string
  (~a #:separator "\n"
      "Options available:"
      "  -h"
      "  --help                  Show this message"
      ""
      "  -c [config]"
      "  --config [config]       Read the config from [config]"
      ""
      "  --                      Read the config from stdin"))

(define (get-config)
  (define json
    (match (current-command-line-arguments)
      [(vector "--") (read-json)]
      [(vector (or "-c" "--config") config-string)
       (call-with-input-string config-string read-json)]
      [(vector (or "-h" "--help")) (raise-user-error help-string)]
      [(vector path) (call-with-input-file* path read-json)]
      [(vector) (raise-user-error (~a "Please pass the config.\n" help-string))]
      [_ (raise-user-error (~a "Unrecognised options.\n" help-string))]))
  (contract r16-config? json
            'config 'config
            'config #f))

(define (make-frontend config)
  (define frontend-config (hash-ref config 'frontend))

  (define frontend-module-string
    (if (string? frontend-config)
        frontend-config
        (hash-ref frontend-config 'module)))

  (define frontend-module
    (with-input-from-string
      frontend-module-string
      read))

  (define make-frontend
    (~> (~a "Frontend " frontend-module " does not provide r16-make-frontend") raise-user-error thunk
        (dynamic-require frontend-module 'r16-make-frontend _)))

  ((contract (-> jsexpr? r16-frontend?) make-frontend
             frontend-module 'frontend
             'frontend #f)
   frontend-config))

(define (main)
  (define config (get-config))
  (define path (hash-ref config 'storage))
  (define db (db:make-trickdb path json->trick))

  (parameterize ([current-backend (new r16% [db db])]
                 [current-frontend (make-frontend config)])
    (send (current-frontend) start)))

(module* main #f
  (main))
