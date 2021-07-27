#lang racket

(provide (struct-out trick) json->trick trick->json)

(struct trick
  (author
   body
   created
   [storage #:mutable]
   [invocations #:mutable]))

(define (trick->json trick)
  (hasheq 'author (trick-author trick)
          'body (trick-body trick)
          'created (trick-created trick)
          'data (trick-storage trick)
          'invocations (trick-invocations trick)))

(define (json->trick json)
  (trick
   (hash-ref json 'author)
   (hash-ref json 'body)
   (hash-ref json 'created)
   (make-hash (hash->list (hash-ref json 'data #hash())))
   (hash-ref json 'invocations)))
