#lang racket

(require gregor
         file/glob
         racket/path
         json
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
  (if (and (regexp-match re-has-http url) (not (regexp-match re-has-pwl url)))
      (list title url)
      #f))

(define (parse-readme path)
  (call-with-input-file path #:mode 'text
    (λ (in)
      (define text (port->string in))
      (define directory (get-paper-dir path))
      (define datetime (timestamp))
      (define match (regexp-match re-markdown-link text))
      (define parsed
        (if (list? match)
            (parse-link-match match)
            #f))
      (if parsed
          (link (first parsed) (last parsed) 0 directory datetime datetime)
          #f))))

; ensure path exists before passing to function
(define (guard-path func)
  (λ (path)
    (if (file-exists? path)
        (func path)
        #f)))

(define (handle-readme logger config state)
  (define REPO-PATH (expand-user-path
                     (hash-ref config "pwlrepo-path")))
  (define SQLITE-PATH (hash-ref config "sqlite-path"))
  (define SQLITE-CONN (hash-ref config "sqlite-conn"))
  (define READMES (glob (build-path REPO-PATH "**" "README.md")))

  state)

(define (process-readmes logger config state)
  (if (equal? (car state) 'error)
      state
      (handle-readmes logger config state)))
