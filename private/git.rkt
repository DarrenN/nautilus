#lang racket/base

(require net/git-checkout
         threading
         "parameters.rkt")

(provide get-repo)

;; Attempt to checkout PWL repo to dest-dir
(define (checkout-repo state)
  (define hostname (hash-ref (current-config) 'pwlrepo-hostname))
  (define repository (hash-ref (current-config) 'pwlrepo-repository))
  (define dest-dir (hash-ref (current-config) 'pwlrepo-path))

  (when (not (directory-exists? dest-dir))
    (make-directory dest-dir))

  (with-handlers
    ([exn:fail:git?
      (λ (e)
        (list 'error
              (format "Couldn't checkout repo ~a/~a" hostname repository)))])

    (define checkout
      (git-checkout hostname repository #:transport 'https #:dest-dir dest-dir))

    (if (string? checkout)
        (append state
                (list (format "Repo ~a/~a checked out" hostname repository)))
        '(error "Did not properly checkout repo!"))))

(define (get-repo state)
  (if (equal? (car state) 'error)
      state
      (checkout-repo state)))
