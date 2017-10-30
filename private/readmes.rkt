#lang racket/base

(require file/glob
         json
         mock
         racket/function
         racket/list
         racket/path
         racket/port
         "db-adapter.rkt"
         "parameters.rkt"
         "pdf.rkt"
         "structs.rkt"
         "utils.rkt")

(provide process-readmes)

(define re-markdown-link #rx"\\[(.*?)\\]\\((.*?)\\)")
(define re-has-http #rx"^(http)")
(define re-has-youtube #rx"youtube")
(define re-has-pwl #rx"(github.com/papers-we-love)")
(define re-has-pdf #rx".pdf$")

; Has http(s) and not github/pwl or youtube
(define (external-url? u)
  (and (not (null? (regexp-match* re-has-http u)))
       (null? (regexp-match* re-has-pwl u))
       (null? (regexp-match* re-has-youtube u))))

; Has .pdf at end
(define (internal-pdf? u)
  (not (null? (regexp-match* re-has-pdf u))))

;; return a link struct
; (struct link (url title directory status created modified) #:transparent)
(define (create-link title url metadata)
  (let ([directory-path (hash-ref metadata 'directory-path)]
        [datetime (hash-ref metadata 'datetime)])
    (link url title (path->string directory-path) 0 datetime datetime)))

; TODO: check url and determine if an internal PDF or external URL then
; pass to another function for processing into the correct struct type
; example: the pdf function should return a file struct with the SHA-1 etc
(define/mock (parse-links matches metadata)
  #:mock create-link #:as create-link-mock
  #:mock create-file #:as create-file-mock

  (filter
   not-false?
   (for/list ([match matches])
     (define url (last match))
     (define title (first match))
     (cond
       [(equal? title ":scroll:") #f]
       [(external-url? url) (create-link title url metadata)]
       [(internal-pdf? url) (create-file title url metadata)]
       [else #f]))))

;; Returns a list of link structs from README markdown
(define (parse-readme repo-path path)
  (λ (in)
    (define-values
      (directory-path file-path is-dir)
      (split-path (find-relative-path repo-path path)))
    (define text (port->string in))
    (define datetime (timestamp))
    (define metadata (hash 'datetime datetime
                           'directory-path directory-path
                           'file-path file-path))

    (define match (regexp-match* re-markdown-link text
                                 #:match-select cdr))

    ; If the README has matches then convert to link or file structs
    (if (list? match)
        (parse-links match metadata)
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
                           (when (link? r) (insert-link logger conn r))
                           (when (pdf? r) (insert-file logger conn r)))))])
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

  (define dt (timestamp))
  (define metadata (hash 'datetime dt
                         'directory-path (string->path "music")
                         'file-path (string->path "README.md")))

  (test-case "external-url? filters out non-http, youtube, and pwl links"
    (check-pred external-url? "https://godflesh.com/streetcleaner/")
    (check-pred external-url? "http://godflesh.com/jesu.pdf")
    (check-false (external-url? "breed-like-rats.pdf"))
    (check-false (external-url? "/godflesh/streetcleaner/breed-like-rats.pdf"))
    (check-false (external-url? "https://www.youtube.com/repoman.pdf"))
    (check-false (external-url? "https://www.github.com/papers-we-love/master/foo.pdf")))

  (test-case "interal-pdf? filters out everything but local pdf files"
    (check-pred internal-pdf? "/godflesh/streetcleaner.pdf")
    (check-pred internal-pdf? "breed-like-rats.ps.pdf")
    (check-false (internal-pdf? "rising.ps")))

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

  (test-case "parse-links differentiates between links and files"
    (with-mocks parse-links
      (with-mock-behavior ([create-link-mock (const 'link)]
                           [create-file-mock (const 'file)])
        (check-equal? (list 'link 'file 'link)
                      (parse-links
                       '(("Wardenclyffe" "http://survive.com/RR7349")
                         ("Sorcerer" "/tx/synth/rr7349.sorcerer.pdf")
                         ("Low Fog"
                          "https://open.spotify.com/track/2N4nQ2ebuyh9fRZxsshH2f"))
                       metadata)))))

  (test-case "parse-links returns title and url if valid pattern"
    (define xs
      (list (link "http://badbrains.com" "Live at CBGB's" "music" 0 dt dt)
            (link "http://dischord.com" "PMA" "music" 0 dt dt)
            (link "https://sst.com" "Big Takeover" "music" 0 dt dt)))
    (define ls
      '(("Live at CBGB's" "http://badbrains.com")
        ("PMA" "http://dischord.com")
        ("Big Takeover" "https://sst.com")))
    (check-equal? xs (parse-links ls metadata)))

  (test-case "parse-links returns null for invalid patterns"
    (with-mocks parse-links
      (with-mock-behavior ([create-link-mock (const 'link)]
                           [create-file-mock (const 'file)])
        (check-equal? null
                      (parse-links
                       '((":scroll:" "http://badbrains.com")
                         ("Live at CBGB's" "/dc/hardcore")
                         ("HR" "https://www.youtube.com/watch?v=2pUlNfdnsAM"))
                       metadata))))))
