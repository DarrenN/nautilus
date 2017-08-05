#lang racket

(require gregor
         file/glob
         racket/path
         json
         "db-adapter.rkt"
         "parameters.rkt"
         "structs.rkt")

(provide process-readmes)

(define re-markdown-link #rx"\\[(.*?)\\]\\((.*?)\\)")
(define re-has-http #rx"^(http)")
(define re-has-pwl #rx"(https://github.com/papers-we-love)")

(define (timestamp)
  (datetime->iso8601 (now/utc)))

;; extract only non-pwl web links from text
(define (parse-link-match match)
  (define url (last match))
  (define title (second match))
  (if (regexp-match re-has-http url)
      (list title url)
      #f))

(define (parse-readme repo-path path)
  (call-with-input-file path #:mode 'text
    (λ (in)
      (define-values
        (directory-path file-path is-dir)
        (split-path (find-relative-path repo-path path)))
      (define text (port->string in))
      (define datetime (timestamp))
      (define match (regexp-match re-markdown-link text))
      (define parsed
        (if (list? match)
            (parse-link-match match)
            #f))
      (if parsed
          (link (last parsed) (first parsed) (path->string directory-path) 0 datetime datetime)
          #f))))

(define (handle-readmes state)
  (define REPO-PATH (expand-user-path
                     (hash-ref (current-config) 'pwlrepo-path)))
  (define SQLITE-PATH (hash-ref (current-config) 'sqlite-path))
  (define SQLITE-CONN (hash-ref (current-config) 'sqlite-conn))
  (define LOGGER (hash-ref (current-config) 'logger))
  (define READMES (glob (build-path REPO-PATH "**" "README.md")))

  (define parsed-readmes
    (filter
     (λ (x) x)
     (for/list ([readme-path READMES])
       (parse-readme REPO-PATH readme-path))))

  ; will be #f is any PDFs cased a non-dupe SQLError
  (define saved-readmes?
    (for/and ([readme parsed-readmes])
      (equal?
       'ok
       (insert-link LOGGER SQLITE-CONN readme))))

  (if saved-readmes?
      (append state '("READMEs processed"))
      '(error "SQLERROR when saving READMES, check logs")))

(define (process-readmes state)
  (if (equal? (car state) 'error)
      state
      (handle-readmes state)))
