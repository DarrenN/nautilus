#lang racket/base

(require file/glob
         mock
         openssl/sha1
         racket/function
         racket/path
         racket/string
         threading
         "db-adapter.rkt"
         "parameters.rkt"
         "structs.rkt"
         "utils.rkt")

(provide create-file)

(define (get-pdf-hash in)
  (sha1 in))

(define/mock (extract-filedata title url metadata repo-path)
  #:mock call-with-input-file #:as call-with-input-mock
  #:with-behavior (const "08b69551734c8dbc2af9690f8a8eca0fcd46cd12")

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

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

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
    (define in (open-input-bytes #"(1 2 3)"))
    (define repo-path "/repo/path/")
    (define metadata
      (hash 'directory-path (string->path "ai/")
            'file-path (string->path "HAL-2000.pdf")
            'datetime (timestamp)))
    (with-mocks extract-filedata
      (define r (extract-filedata "HAL 2000 and You"
                                  "hal-2000.pdf"
                                  metadata
                                  repo-path))

      (check-equal? (pdf-sha1 r) "08b69551734c8dbc2af9690f8a8eca0fcd46cd12")
      (check-equal? (pdf-filename r) "HAL-2000.pdf")
      (check-equal? (pdf-directory r) "ai/")
      (check-regexp-match re-date (pdf-created r))
      (check-regexp-match re-date (pdf-modified r)))))
