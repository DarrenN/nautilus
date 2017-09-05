#lang racket/base

(require gregor
         racket/bool
         racket/match)

(provide (all-defined-out))

(define (timestamp)
  (datetime->iso8601 (now/utc)))

;; destructure lists into values
(define (get-values xs)
  (match xs
    [(list a b) (values a b)]
    [(list a b c) (values a b c)]))

;; Predicate for use in filter, etc
(define not-false? (compose not false?))

;; Get last item in vector
(define (vector-last v)
  (vector-ref v (- (vector-length v) 1)))
