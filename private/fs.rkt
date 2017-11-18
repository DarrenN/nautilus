#lang racket/base

(require json
         mock
         net/url
         racket/function
         racket/list
         racket/path
         racket/port
         threading
         "directories.rkt"
         "parameters.rkt"
         "threads.rkt"
         "utils.rkt")

(provide walk-dirs)

(define WORKERS 10)

(define readme-md "README.md")
(define metadata-md "METADATA.md")
(define metadata-json "metadata.json")

(define work-channel (make-channel))
(define counter-channel (make-channel))

(define re-markdown-link #rx"\\[(.*?)\\]\\((.*?)\\)")
(define re-has-http #rx"^(http)")
(define re-has-youtube #rx"youtube")
(define re-has-pwl #rx"(github.com/papers-we-love)")
(define re-has-pdf #rx".pdf$")

(define (scroll? s)
  (equal? s ":scroll:"))

(define (youtube? s)
  (not (null? (regexp-match* re-has-youtube s))))

;; Sit on the result thread and print results until all the DONEs are in
(define count-result-thread (create-done-counter WORKERS))

;; Dispatch path from thread to handle-directory
;; (-> list (U symbol string))
(define (worker-proc result)
  (define dir (first result))
  (define repo-path (last result))
  (if (equal? dir 'DONE)
      'TERMINATE
      (handle-directory dir repo-path)))

;; [IO] Find README files in a dir, assemble metadata and process
;; (-> string string string)
(define (handle-directory dir-path repo-path)
  (define dirs (get-directories dir-path))
  (define readme-path (build-path dir-path readme-md))
  (define metadata-path (build-path dir-path metadata-json))
  (define path-hash (hasheq 'dir-path dir-path 'repo-path repo-path 'dirs dirs))

  (define res
    (cond
      [(file-exists? readme-path)
       (format "Readme Found: ~a" (create-metadata-from-readme path-hash))]
      [else (format "Not Data File Found!: ~a" metadata-path)]))

  res)

;; Pipeline README from markdown to metadata.json
;; (-> hasheq hasheq)
(define (create-metadata-from-readme path-hash)
  (~> path-hash
      create-metadata-hash
      write-metadata-json))

;; Walk map over a list of links and create a hash of title, link
;; for valid URLs
;; (-> ((string)) (hasheq))
(define (create-links-hash links)
  (define github-base-url
    (format "https://~a" (hash-ref (current-config) 'pwlrepo-hostname)))
  (filter
   not-false?
   (for/list ([link links])
     (define raw-url (last link))
     (define url
       (if (is-valid-url? raw-url)
           raw-url
           (url->string
            (create-github-blob-url github-base-url raw-url))))
     (define title (first link))
     (cond
       [(or (scroll? title) (youtube? url)) #f]
       [else (hash 'title title 'url url)]))))

;; [IO] Convert readme into a hash we can use for JSON
;; (-> hasheq hasheq)
(define (create-metadata-hash paths)
  (define dir-path (hash-ref paths 'dir-path))
  (define repo-path (hash-ref paths 'repo-path))
  (define dirs (hash-ref paths 'dirs))
  (define relative-dir (find-relative-path repo-path dir-path))
  (define subs
    (map (λ (p) (path->string (find-relative-path repo-path p))) dirs))
  (define raw-links (call-with-input-file (build-path dir-path readme-md)
                      #:mode 'text get-readme-links))
  (define links (create-links-hash raw-links))

  (hasheq 'directory dir-path
          'relative-directory (path->string relative-dir)
          'subdirectories subs
          'links links))

;; [IO] Write metadata hash to metadata.json file in directory
;; (-> hasheq hasheq)
(define (write-metadata-json metadata)
  (define dir-path (hash-ref metadata 'directory))
  (define target-path (build-path dir-path metadata-json))
  (define json (hash-remove
                (hash-set metadata 'directory
                          (hash-ref metadata 'relative-directory))
                'relative-directory))

  (call-with-output-file* target-path #:mode 'text #:exists 'replace
    (λ (out) (write-json json out #:encode 'all)))

  metadata)

;; [IO] Consume a string port and extract Markdown links via regex
;; (-> port list)
(define (get-readme-links in)
  (define text (port->string in))
  (regexp-match* re-markdown-link text
                 #:match-select cdr))

;; Pull up the repo directories and feed to worker threads
(define (recurse-dirs out)
  (define repo-path (hash-ref (current-config) 'pwlrepo-path))
  (define directories
    (append (get-directories repo-path)
            (make-list WORKERS 'DONE)))

  ;; Passes worker results to out and keeps tabs on DONE states
  (define counter-thread
    (create-channel-interceptor counter-channel out count-result-thread))

  ;; Create worker threads to process paths
  (define work-threads
    (create-interceptor-pool WORKERS work-channel counter-channel worker-proc))

  ;; Push directory paths into work channel
  (for ([d directories])
    (channel-put work-channel (list d repo-path))))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define (walk-dirs out)
  (recurse-dirs out))

;//////////////////////////////////////////////////////////////////////////////
; TESTS
