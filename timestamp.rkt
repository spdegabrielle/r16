#lang racket/base

(require
 (only-in racket/date date->seconds)
 racket/string
 (only-in srfi/19 string->date))

(provide discord-timestamp-to-unix)

;; Discord timestamps are in the format 2022-02-22T14:42:15.223333+00:00.
;; We assume and verify that the zone is always UTC, then strip it off,
;; as srfi-19 cannot parse it.
(define (discord-timestamp-to-unix s)
  (unless (string-suffix? s "+00:00")
    (raise (exn:fail "Expect timestamps with UTC zone" (current-continuation-marks))))
  ;; Anything extra in the string that is not specified in the template
  ;; (aka the subsecond precision and +00:00 zone) is ignored by string->date
  (date->seconds (string->date s "~Y-~m-~dT~H:~M:~S") #f))
