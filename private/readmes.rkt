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
(define re-has-youtube #rx"youtube")
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
  (λ (in)
    (define-values
      (directory-path file-path is-dir)
      (split-path (find-relative-path repo-path path)))
    (define text (port->string in))
    (define datetime (timestamp))
    (define match (regexp-match* re-markdown-link text
                                 #:match-select cdr))
    (define parsed (if (list? match)
                       (parse-links match)
                       #f))

    (if parsed
        (for/list ([p parsed]
                   #:when p)
          (link (last p) (first p) (path->string directory-path)
                0 datetime datetime))
        #f)))

(define (handle-readmes state)
  (define repo-path (expand-user-path
                     (hash-ref (current-config) 'pwlrepo-path)))
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define logger (hash-ref (current-config) 'logger))
  (define readmes (glob (build-path repo-path "**" "README.md")))

  (define parsed-readmes
    (filter
     not-false?
     (for/list ([readme-path readmes])
         (call-with-input-file readme-path #:mode 'text
           (parse-readme repo-path readme-path)))))

  ; will be #f is any PDFs cased a non-dupe SQLError
  (define saved-readmes?
    (for/and ([insert (flatten
                       (for/list ([readme parsed-readmes])
                         (for/list ([r readme])
                           (insert-link logger conn r))))])
      (equal? 'ok insert)))

  (if saved-readmes?
      (append state '("READMEs processed"))
      '(error "SQLERROR when saving links, check logs")))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define (process-readmes state)
  (if (equal? (car state) 'error)
      state
      (handle-readmes state)))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit)

  (test-case "parse-readme returns a list of link structs from an input-port"
    (define re-date
      #px"^[\\d]{4}-[\\d]{2}-[\\d]{2}T[\\d]{2}:[\\d]{2}:[\\d]{2}.[\\d]*")
    (define in (open-input-string "# Raw Power
     * [Search and Destroy](https://en.wikipedia.org/wiki/Raw_Power)
     * [Gimme Danger](https://en.wikipedia.org/wiki/The_Stooges)"))
    (define readfn
      (parse-readme
       (string->path "papers-we-love/papers-we-love")
       (string->path "papers-we-love/papers-we-love/plt/README.md")))

    (define ls (readfn in))
    (for ([l ls])
      (check-pred link? l)
      (check-regexp-match "https://en.wikipedia.org" (link-url l))
      (check-regexp-match "Search and Destroy|Gimme Danger" (link-title l))
      (check-equal? "plt/" (link-directory l))
      (check-equal? 0 (link-status l))
      (check-regexp-match re-date (link-created l))
      (check-regexp-match re-date (link-modified l))))

  (test-case "process-readmes let's 'error states fall through"
    (define err '(error ("Bad things went down")))
    (check-equal? err (process-readmes err)))

  (test-case "parse-links returns title and url if valid pattern"
    (define xs '(("Live at CBGB's" "http://badbrains.com")
                 ("PMA" "http://dischord.com")
                 ("Big Takeover" "https://sst.com")))
    (check-equal? xs (parse-links xs)))

  (test-case "parse-links returns null for invalid patterns"
    (check-equal? null
      (parse-links '((":scroll:" "http://badbrains.com")
                     ("Live at CBGB's" "/dc/hardcore")
                     ("HR" "https://www.youtube.com/watch?v=2pUlNfdnsAM"))))))
