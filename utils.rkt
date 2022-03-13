#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/contract)

(provide thread-loop
         make-expiring-cache
         expiring-cache-purge
         expiring-cache-get)

(define-syntax (thread-loop stx)
  (syntax-parse stx
    [(_ body:expr ...)
     (syntax/loc stx
       (let ()
         (define (loop)
           body ...
           (loop))
         (thread loop)))]))

(struct expiring-cache
  (timestamp-getter
   compute-value ; k -> v
   entries-lock
   entries ; hash, k -> (timestamp . v)
   ttl))

; NB: ttl and timestamp-getter can be in any units, as long as they are consistent with each other
(define/contract (make-expiring-cache timestamp-getter compute-value ttl)
  (-> (-> real?) (-> any/c any/c) real? expiring-cache?)
  (expiring-cache
   timestamp-getter
   compute-value
   (make-semaphore 1)
   (make-hash)
   ttl))

; purge stale entries from the cache, returning the list of keys purged
; Call at some regular interval.
(define/contract (expiring-cache-purge cache)
  (-> expiring-cache? list?)
  (define now ((expiring-cache-timestamp-getter cache)))
  (define (is-stale timestamp)
    (> (- now timestamp)
       (expiring-cache-ttl cache)))
  (call-with-semaphore
   (expiring-cache-entries-lock cache)
   (lambda ()
     (define entries (expiring-cache-entries cache))
     (define keys-to-remove
       (for/fold ([acc null])
                 ([(key timestamp-value) (in-hash entries)]
                  #:when (is-stale (car timestamp-value)))
         (cons key acc)))
     (for ([key (in-list keys-to-remove)])
       (hash-remove! entries key))
     keys-to-remove)))

;; get the cached entry for k, computing it if not present.
;; if k is already in cache, its expiration timer is refreshed.
(define/contract (expiring-cache-get cache k)
  (-> expiring-cache? any/c any/c)
  (call-with-semaphore
   (expiring-cache-entries-lock cache)
   (lambda ()
     (define entries (expiring-cache-entries cache))
     (if (hash-has-key? entries k)
         ;; bump the ttl
         (let ()
           (hash-update! entries k
                         (lambda (old)
                           (define now ((expiring-cache-timestamp-getter cache)))
                           (cons now (cdr old))))
           (cdr (hash-ref entries k)))
         ;; compute the value
         (let* ([value ((expiring-cache-compute-value cache) k)]
                ;; compute timestamp after the value since computing the value could
                ;; take a long time (network IO, etc.)
                [now ((expiring-cache-timestamp-getter cache))])
           (hash-set! entries k (cons now value))
           value)))))

(module+ test
  (require rackunit)
  (test-case "Smoke Test"
    (define ttl 5)
    (define fake-current-timestamp (box 0))
    (define times-compute-value-called (box 0))
    (define (compute-value k)
      (set-box! times-compute-value-called (add1 (unbox times-compute-value-called)))
      (add1 k))

    (define cache (make-expiring-cache
                   (lambda () (unbox fake-current-timestamp))
                   compute-value
                   ttl))
    ;; technically we're accessing this without taking the lock,
    ;; but the test is single threaded so whatever.
    (define entries (expiring-cache-entries cache))

    (check-eqv? 1 (expiring-cache-get cache 0))
    (check-eqv? 1 (unbox times-compute-value-called))

    (set-box! fake-current-timestamp 1)
    (check-eqv? 2 (expiring-cache-get cache 1))
    (check-eqv? 2 (unbox times-compute-value-called))

    (check-true (hash-has-key? entries 0) "Key 0 should still be cached")

    (set-box! fake-current-timestamp (+ ttl (unbox fake-current-timestamp)))
    (check-equal? '(0) (expiring-cache-purge cache) "Key 0 should be purged")

    (check-true (hash-has-key? entries 1) "Key 1 should still be cached")

    (set-box! fake-current-timestamp (add1 (unbox fake-current-timestamp)))
    (check-equal? '(1) (expiring-cache-purge cache) "Key 1 should be purged"))

  (test-case "Expiration timer refresh"
    (define ttl 5)
    (define fake-current-timestamp (box 0))
    (define times-compute-value-called (box 0))
    (define (compute-value k)
      (set-box! times-compute-value-called (add1 (unbox times-compute-value-called)))
      (add1 k))

    (define cache (make-expiring-cache
                   (lambda () (unbox fake-current-timestamp))
                   compute-value
                   ttl))

    ;; Fetch key 0 and populate the cache
    (check-eqv? 1 (expiring-cache-get cache 0))
    (check-eqv? 1 (unbox times-compute-value-called))

    ;; Advance one time unit and fetch it again, this should hit in cache, but update the ttl
    (set-box! fake-current-timestamp (add1 (unbox fake-current-timestamp)))
    (check-eqv? 1 (expiring-cache-get cache 0))
    (check-eqv? 1 (unbox times-compute-value-called) "Should have hit in cache")

    ;; Advance to when key *would have* been purged if we hadn't touched it a second time
    (set-box! fake-current-timestamp (+ ttl (unbox fake-current-timestamp)))
    (check-equal? null (expiring-cache-purge cache) "We touched Key 0 at a later time, so it shouldn't be purged yet")

    ;; Advance one last time unit and this time should be purged
    (set-box! fake-current-timestamp (add1 (unbox fake-current-timestamp)))
    (check-equal? '(0) (expiring-cache-purge cache) "Should be purged")))
