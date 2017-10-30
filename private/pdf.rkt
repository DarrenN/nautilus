#lang racket/base

(require file/glob
         openssl/sha1
         racket/path
         racket/string
         threading
         "db-adapter.rkt"
         "parameters.rkt"
         "structs.rkt"
         "utils.rkt")

(provide process-pdfs
         create-file)

(define (get-pdf-hash in)
  (sha1 in))

(define (extract-filedata title url metadata repo-path)
  (define pdf-path (build-path repo-path
                               (hash-ref metadata 'directory-path)
                               (hash-ref metadata 'file-path)))

  (define filehash (call-with-input-file
                     pdf-path #:mode 'binary
                     get-pdf-hash))

  (define datetime (hash-ref metadata 'datetime))

  (pdf filehash
       title
       (path->string (hash-ref metadata 'file-path))
       (path->string (hash-ref metadata 'directory-path))
       datetime
       datetime))

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
      (equal? 'ok
              (insert-file
               LOGGER SQLITE-CONN
               (call-with-input-file pdf-path #:mode 'binary
                 (extract-filedata REPO-PATH pdf-path))))))

  (if saved-pdfs?
      (append state '("PDFs processed"))
      '(error "SQLERROR when saving PDFs, check logs")))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define (process-pdfs state)
  (if (equal? (car state) 'error)
      state
      (handle-pdfs state)))

(define (create-file title url metadata)
  (define REPO-PATH (expand-user-path
                     (hash-ref (current-config) 'pwlrepo-path)))
  (extract-filedata title url metadata REPO-PATH))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit)

  (test-case "extract-filedata returns a pdf struct from an input-port"
    (define re-date
      #px"^[\\d]{4}-[\\d]{2}-[\\d]{2}T[\\d]{2}:[\\d]{2}:[\\d]{2}.[\\d]*")
    (define in (open-input-bytes #"(i got a letter from the government)"))
    (define readfn (extract-filedata
                    (string->path "papers-we-love/papers")
                    (string->path "papers-we-love/papers/ai/HAL-2000.pdf")))
    (define p (readfn in))
    (check-equal? (pdf-sha1 p) "07eba1b33856e41e2f99e8aead30762a41da3ca3")
    (check-equal? (pdf-filename p) "HAL-2000.pdf")
    (check-equal? (pdf-directory p) "ai/")
    (check-regexp-match re-date (pdf-created p))
    (check-regexp-match re-date (pdf-modified p)))

  (test-case "process-pdfs let's error state fall through"
    (define err '(error ("Bad things went down")))
    (check-equal? err (process-pdfs err))))
