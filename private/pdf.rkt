#lang racket/base

(require file/glob
         gregor
         openssl/sha1
         racket/path
         racket/string
         threading
         "db-adapter.rkt"
         "parameters.rkt"
         "structs.rkt")

(provide process-pdfs)

(define (timestamp)
  (datetime->iso8601 (now/utc)))

(define (normalize-filename file-path)
  (~> file-path
      (path-replace-extension "")
      path->string
      string-downcase
      (string-replace  #rx"-|_" " ")
      string-normalize-spaces))

(define (extract-filedata repo-path path)
  (call-with-input-file path #:mode 'binary
    (λ (in)
      (define-values
        (directory-path file-path is-dir)
        (split-path (find-relative-path repo-path path)))
      (define datetime (timestamp))
      (pdf (sha1 in)
           (path->string file-path)
           (path->string directory-path)
           (normalize-filename file-path)
           datetime
           datetime))))

(define (handle-pdfs state)
  (define REPO-PATH (expand-user-path
                     (hash-ref (current-config) 'pwlrepo-path)))
  (define SQLITE-PATH (hash-ref (current-config) 'sqlite-path))
  (define SQLITE-CONN (hash-ref (current-config) 'sqlite-conn))
  (define PDFS (glob (build-path REPO-PATH "**" "*.pdf")))
  (define LOGGER (hash-ref (current-config) 'logger))

  ; will be #f is any PDFs cased a non-dupe SQLError
  (define saved-pdfs?
    (for/and ([pdf-path PDFS])
      (equal?
       'ok
       (insert-file LOGGER SQLITE-CONN (extract-filedata REPO-PATH pdf-path)))))

  (if saved-pdfs?
      (append state '("PDFs processed"))
      '(error "SQLERROR when saving PDFs, check logs")))

(define (process-pdfs state)
  (if (equal? (car state) 'error)
      state
      (handle-pdfs state)))
