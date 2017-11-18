#lang racket/base

(require gregor
         net/url-string
         racket/bool
         racket/list
         racket/match
         racket/string)

(provide (all-defined-out))

(define GITHUB-BLOB-PATH "/blob/master/")

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

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

;; state falls through if it's an error
(define (guard-state fn state)
  (if (equal? (car state) 'error)
      state
      (fn state)))

;; Is this a valid web url?
(define (is-valid-url? url)
  (regexp-match? #px"^(http|https)://" url))

;; Attempt to convert a relative file path into a GitHub blob path
(define (create-github-blob-url base url)
  (let ([blob-url (combine-url/relative (string->url base) GITHUB-BLOB-PATH)])
    (combine-url/relative blob-url (string-replace url #rx"^\\.\\./|^/" ""))))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit)
  (define re-date
    #px"^[\\d]{4}-[\\d]{2}-[\\d]{2}T[\\d]{2}:[\\d]{2}:[\\d]{2}.[\\d]*")

  (test-case "create-github-blob-url"
    (check-equal? (create-github-blob-url "https://svii.com/" "our-time.pdf")
                  (string->url "https://svii.com/blob/master/our-time.pdf"))
    (check-equal? (create-github-blob-url "https://svii.com/" "/your-time.pdf")
                  (string->url "https://svii.com/blob/master/your-time.pdf"))
    (check-equal? (create-github-blob-url "https://svii.com/" "../foo/your-time.pdf")
                  (string->url "https://svii.com/blob/master/foo/your-time.pdf")))

  (test-case "is-valid-url?"
    (check-pred is-valid-url? "http://gooogle.com")
    (check-pred is-valid-url? "https://gooogle.com")
    (check-false (is-valid-url? "https:gooogle.com"))
    (check-false (is-valid-url? "../arya/sansa")))

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
