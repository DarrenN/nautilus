#lang racket/base

(require net/git-checkout
         racket/system
         threading
         "logger.rkt"
         "parameters.rkt"
         "utils.rkt")

(provide get-repo
         push-repo)

;; Attempt to checkout PWL repo to dest-dir
(define (checkout-repo logger state)
  (define hostname (hash-ref (current-config) 'pwlrepo-hostname))
  (define repository (hash-ref (current-config) 'pwlrepo-repository))
  (define dest-dir (hash-ref (current-config) 'pwlrepo-path))
  (define tmp-branch (hash-ref (current-config) 'tmp-branch))

  (define cmd-gitclone (format "git clone git@github.com:~a ~a"
                               repository dest-dir))

  (define cmd-checkoutmaster "git checkout master")
  (define cmd-pull-ff "git pull --ff-only")
  (define cmd-branch-nautilus (format "git checkout -b ~a" tmp-branch))

  ;; If dir doesn't exist then clone the repo into it
  (when (not (directory-exists? dest-dir))
    (system cmd-gitclone))

  ;; Switch into the dest-dir, pull the lastest master and branch off
  (current-directory dest-dir)

  (define cmd-result
    (for/and ([cmd (list cmd-checkoutmaster cmd-pull-ff cmd-branch-nautilus)])
        (system cmd)))

  (if cmd-result
      (begin
        (logger "~a" (format "GITBRANCH ~a CHECKED OUT" tmp-branch))
        (append state
                (list (format "Repo ~a/~a checked out" hostname repository))))
      (begin
        (logger "~a" (format "GITERR ~a CHECK OUT PROBLEM to ~a" tmp-branch
                             dest-dir))
        '(error "Did not properly checkout repo!"))))

(define (push-and-close-repo state)
  (define hostname (hash-ref (current-config) 'pwlrepo-hostname))
  (define repository (hash-ref (current-config) 'pwlrepo-repository))
  (define dest-dir (hash-ref (current-config) 'pwlrepo-path))
  (define logger (hash-ref (current-config) 'logger))
  (define tmp-branch (hash-ref (current-config) 'tmp-branch))

  (define cmd-add-all "git add .")
  (define cmd-commit (format "git commit -a -m 'nautilus.db from branch ~a'"
                             tmp-branch))
  (define cmd-merge (format "git checkout master; git merge ~a" tmp-branch))
  (define cmd-push "git push")
  (define cmd-delete (format "git branch -D ~a" tmp-branch))

  (define cmd-result
    (for/and ([cmd (list cmd-add-all cmd-commit cmd-merge cmd-push cmd-delete)])
      (system cmd)))

  (if cmd-result
      (begin
        (logger "~a" (format "GITBRANCH ~a MERGED / DELETED" tmp-branch))
        (append state
                (list (format "Repo ~a/~a merged and branch deleted"
                              hostname repository))))
      (begin
        (logger "~a" (format "GITERR ~a MERGE / PUSH PROBLEM IN ~a" tmp-branch
                             dest-dir))
        '(error "Could not properly merge/cleanup repo"))))

(define (get-repo logger state)
  (if (equal? (car state) 'error)
      state
      (checkout-repo logger state)))

(define (push-repo state)
  (if (equal? (car state) 'error)
      state
      (push-and-close-repo state)))
