#lang racket/base

(require file/glob
         json
         racket/list
         racket/path
         racket/port
         "db-adapter.rkt"
         "parameters.rkt"
         "structs.rkt"
         "utils.rkt")

(provide process-readmes)

(define re-markdown-link #rx"\\[(.*?)\\]\\((.*?)\\)")
(define re-has-http #rx"^(http)")
(define re-has-youtube #rx"^(youtube)")
(define re-has-pwl #rx"(https://github.com/papers-we-love)")

;; extract only non-pwl web links from text
(define (parse-links matches)
  (filter
   not-false?
   (for/list ([match matches])
     (define url (last match))
     (define title (first match))
     (if (and
          (not (equal? title ":scroll:"))
          (not (regexp-match re-has-youtube url))
          (regexp-match re-has-http url))
         (list title url)
         #f))))

;; Returns a list of link structs from README markdown
(define (parse-readme repo-path path)
  (call-with-input-file path #:mode 'text
    (λ (in)
      (define-values
        (directory-path file-path is-dir)
        (split-path (find-relative-path repo-path path)))
      (define text (port->string in))
      (define datetime (timestamp))
      (define match (regexp-match* re-markdown-link text
                                   #:match-select cdr))
      (define parsed
        (if (list? match)
            (parse-links match)
            #f))
      (if parsed
          (for/list ([p parsed]
                     #:when p)
            (link (last p) (first p) (path->string directory-path)
                  0 datetime datetime))
          #f))))

(define (handle-readmes state)
  (define REPO-PATH (expand-user-path
                     (hash-ref (current-config) 'pwlrepo-path)))
  (define SQLITE-CONN (hash-ref (current-config) 'sqlite-conn))
  (define LOGGER (hash-ref (current-config) 'logger))
  (define READMES (glob (build-path REPO-PATH "**" "README.md")))

  (define parsed-readmes
    (filter
     not-false?
     (for/list ([readme-path READMES])
       (parse-readme REPO-PATH readme-path))))

  ; will be #f is any PDFs cased a non-dupe SQLError
  (define saved-readmes?
    (for/and ([insert (flatten
                       (for/list ([readme parsed-readmes])
                         (for/list ([r readme])
                           (insert-link LOGGER SQLITE-CONN r))))])
      (equal? 'ok insert)))

  (if saved-readmes?
      (append state '("READMEs processed"))
      '(error "SQLERROR when saving READMES, check logs")))

(define (process-readmes state)
  (if (equal? (car state) 'error)
      state
      (handle-readmes state)))
