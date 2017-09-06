#lang racket/base

(require net/git-checkout
         racket/port
         racket/system
         threading
         "logger.rkt"
         "parameters.rkt"
         "utils.rkt")

(provide get-repo
         push-repo)

(define re-commit-clean #rx"working tree clean")

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

(define (check-clean logger tmp-branch cmd state)
  (define was-clean? (regexp-match
                      re-commit-clean
                      (with-output-to-string (lambda () (system cmd)))))

  ;; Kill the branch
  (system (format "git checkout master; git branch -D ~a" tmp-branch))

  (if was-clean?
        (logger "~a" (format "GITBRANCH ~a CLEAN! - NO MERGE" tmp-branch))
        (logger "~a" (format "GITBRANCH ~a ERROR! - NO MERGE" tmp-branch)))
  (append state
          (list (format "Branch ~a not merged" tmp-branch))))

(define (merge-push-delete logger tmp-branch dest-dir cmds state)
  (define cmd-result
    (for/and ([cmd cmds])
      (system cmd)))
  (if cmd-result
      (begin
        (append state (list (format "Branch ~a merged and branch deleted"
                                    tmp-branch))))
      (begin
        (logger "~a" (format "GITERR ~a MERGE / PUSH PROBLEM IN ~a" tmp-branch
                             dest-dir))
        '(error "Could not properly merge/cleanup repo"))))

(define (push-and-close-repo state)
  (define hostname (hash-ref (current-config) 'pwlrepo-hostname))
  (define repository (hash-ref (current-config) 'pwlrepo-repository))
  (define dest-dir (hash-ref (current-config) 'pwlrepo-path))
  (define logger (hash-ref (current-config) 'logger))
  (define tmp-branch (hash-ref (current-config) 'tmp-branch))

  (define cmd-add-all "git add .") ; always #t
  (define cmd-commit (format "git commit -a -m 'nautilus.db from branch ~a'"
                             tmp-branch)) ; get string and check it
  (define cmd-merge (format "git checkout master; git merge ~a" tmp-branch))
  (define cmd-push "git push")
  (define cmd-delete (format "git branch -D ~a" tmp-branch))

  (system cmd-add-all) ;; add always returns #t

  (define did-commit? (system cmd-commit)) ;; try to commit

  (if did-commit?
      (merge-push-delete logger tmp-branch dest-dir
                         (list cmd-merge cmd-push cmd-delete) state)
      (check-clean logger tmp-branch cmd-commit state)))

(define (get-repo logger state)
  (if (equal? (car state) 'error)
      state
      (checkout-repo logger state)))

(define (push-repo state)
  (if (equal? (car state) 'error)
      state
      (push-and-close-repo state)))
