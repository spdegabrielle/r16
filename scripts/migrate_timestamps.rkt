#lang racket/base

;; This script migrates the timestamps stored in a save data json file from
;; Discord's ISO8601 variant to unix timestamps. Reads from stdin and outputs to stdout.
;; Establishes a proper common format in the save data json across frontends
;; and overall storing numerical timestamps is saner.

(require json "../timestamp.rkt")

(define (main)
  (define blob (read-json))
  (define updated-blob
    (for/hash ([(name trick) (in-hash blob)])
      (values name (hash-update trick 'created discord-timestamp-to-unix))))
  (write-json updated-blob))

(module* main #f
  (main))
