#lang racket/base

(require gregor
         racket/bool
         racket/list
         racket/match)

(provide (all-defined-out))

(define (timestamp)
  (datetime->iso8601 (now/utc)))

(define (timestamp-posix)
  (exact->inexact (->posix (now))))

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

;; merge hash b into a
(define (hash-merge a b)
  (for/hash ([key (remove-duplicates (append (hash-keys a) (hash-keys b)))])
    (cond [(hash-has-key? b key) (values key (hash-ref b key))]
          [(hash-has-key? a key) (values key (hash-ref a key))])))

(module+ test
  (require rackunit)
  (define re-date
    #px"^[\\d]{4}-[\\d]{2}-[\\d]{2}T[\\d]{2}:[\\d]{2}:[\\d]{2}.[\\d]*")

  (test-case "hash-merge"
    (define a (hash 'a 1 'b 2 'c 3 'd 4 'f 22))
    (define b (hash 'a 1 'c 12 'e 44))
    (check-equal? (hash-merge a b) (hash 'a 1 'b 2 'c 12 'd 4 'e 44 'f 22)))

  (test-case "vector-last"
    (check-equal? 12 (vector-last (vector 1 2 3 45 65 12))))

  (test-case "not-false?"
    (check-true (not-false? '(1 2)))
    (check-true (not-false? '()))
    (check-true (not-false? #t))
    (check-false (not-false? #f)))

  (test-case "get-values"
    (define-values (a b) (get-values '(1 2)))
    (define-values (c d e) (get-values '(3 4 5)))
    (check-equal? (list a b c d e) '(1 2 3 4 5)))

  (test-case "timestamp returns an ISO8601 date"
    (check-regexp-match re-date (timestamp)))

  (test-case "timestamp-posix returns an epoch date"
    (check-pred positive? (timestamp-posix))
    (check-pred flonum? (timestamp-posix))
    (check-regexp-match re-date
                        (datetime->iso8601
                         (posix->datetime (timestamp-posix))))))
