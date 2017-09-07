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

(provide process-pdfs)

(define (normalize-filename file-path)
  (~> file-path
      (path-replace-extension "")
      path->string
      string-downcase
      (string-replace  #rx"-|_" " ")
      string-normalize-spaces))

(define (extract-filedata repo-path path)
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
           datetime)))

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

(define (process-pdfs state)
  (if (equal? (car state) 'error)
      state
      (handle-pdfs state)))

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
    (check-equal? (pdf-normalized p) "hal 2000")
    (check-regexp-match re-date (pdf-created p))
    (check-regexp-match re-date (pdf-modified p)))

  (test-case "process-pdfs let's error state fall through"
    (define err '(error ("Bad things went down")))
    (check-equal? err (process-pdfs err)))

  (test-case "normalize file name does what it says"
    (check-equal? (normalize-filename (string->path "/sabrina/the/witch.exe"))
                  "/sabrina/the/witch")
    (check-equal? (normalize-filename (string->path "/Sabrina/tHe/WiTcH.exeE"))
                  "/sabrina/the/witch")
    (check-equal? (normalize-filename (string->path "Sabrina-the_witch.pdf"))
                  "sabrina the witch")
    (check-equal? (normalize-filename (string->path "   sabrina The   witch"))
                  "sabrina the witch")))
