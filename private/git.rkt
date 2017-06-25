#lang racket

(require net/git-checkout
         threading)

(provide get-repo)

;; Attempt to checkout PWL repo to dest-dir
(define (checkout-repo logger config state)
  (define hostname (hash-ref config "pwlrepo-hostname"))
  (define repository (hash-ref config "pwlrepo-repository"))
  (define dest-dir (hash-ref config "pwlrepo-path"))

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

(define (get-repo logger config state)
  (if (equal? (car state) 'error)
      state
      (checkout-repo logger config state)))
