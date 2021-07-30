#!/usr/bin/env racket
#lang racket/base

(require
 (only-in racket/class new send)
 (only-in racket/cmdline parse-command-line)
 (only-in racket/contract -> contract or/c)
 (only-in racket/format ~a)
 (only-in racket/function const thunk)
 (only-in racket/port call-with-input-string with-input-from-string)
 json
 "backend.rkt"
 "common.rkt"
 "config.rkt"
 "log.rkt"
 "interface.rkt"
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

(define (get-config)
  (parse-command-line
   "r16"
   (current-command-line-arguments)
   ; flag definitions
   `((usage-help
      "R16: Interactive, Community-Driven Code Evaluation")
     (once-any
      [("-c" "--config")
       ,(lambda (_flag path)
          (if (equal? path "-")
              (read-json)
              (call-with-input-file* path read-json)))
       ("Path to config file. If `-`, config is read as json from standard input." "path")]
      [("-s" "--config-string")
       ,(lambda (_flag config) (call-with-input-string config read-json))
       ("Provide config on the command line as a json string." "config_json")]))
   ; Receives flag values + positional arguments
   ; Result of this function is the result of the whole parse-command-line form.
   (lambda (flag-values)
     (contract r16-config? (car flag-values)
               'config 'config
               'config #f))
   ; positional argument names
   '()))

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
    (dynamic-require
     frontend-module
     'r16-make-frontend
     (thunk (raise-user-error
             (~a "Frontend " frontend-module " does not provide r16-make-frontend")))))

  ((contract (-> jsexpr? r16-frontend?) make-frontend
             frontend-module 'frontend
             'frontend #f)
   frontend-config))

(define (main)
  (define config (get-config))
  (define path (hash-ref config 'storage))
  (define db (db:make-trickdb path json->trick))

  (define r16-receiver (make-log-receiver r16-logger 'debug))
  (thread
   (thunk
    (let loop ()
      (let ([v (sync r16-receiver)])
        (printf "[~a] ~a\n"
                (vector-ref v 0)
                (vector-ref v 1)))
      (loop))))

  (parameterize ([current-backend (new r16% [db db])]
                 [current-frontend (make-frontend config)])
    (send (current-frontend) start)))

(module* main #f
  (main))
