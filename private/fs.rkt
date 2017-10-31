#lang racket/base

(require json
         mock
         racket/function
         racket/list
         racket/path
         racket/port
         threading
         "parameters.rkt"
         "threads.rkt"
         "utils.rkt")

(provide walk-dirs)

(define WORKERS 10)

(define readme-md "README.md")
(define metadata-md "METADATA.md")
(define metadata-json "metadata.json")

(define done-counter (box 0))
(define work-channel (make-channel))
(define result-channel (make-channel))

(define re-markdown-link #rx"\\[(.*?)\\]\\((.*?)\\)")
(define re-has-http #rx"^(http)")
(define re-has-youtube #rx"youtube")
(define re-has-pwl #rx"(github.com/papers-we-love)")
(define re-has-pdf #rx".pdf$")

(define (scroll? s)
  (equal? s ":scroll:"))

(define (youtube? s)
  (not (null? (regexp-match* re-has-youtube s))))

;; Keep tabs on how many 'DONE signals have come in
(define (count-done-threads d)
  (let ([ov (unbox done-counter)])
    (when (equal? d 'DONE) (box-cas! done-counter ov (add1 ov)))
    (unbox done-counter)))

;; Find the worker by list-ref and nuke
(define (kill-worker id)
  (let* ([ths (unbox work-threads)]
         [th (list-ref ths id)])
    (when (and (thread? th) (thread-running? th)) (kill-thread th))))

;; Dispatch path from thread to handle-directory
(define (worker-proc result)
  (define dir (first result))
  (define repo-path (last result))
  (if (equal? dir 'DONE)
      'TERMINATE
      (handle-directory dir repo-path)))

;; Path is a dir and not git related
(define (valid-dir? p)
  (and (directory-exists? p)
       (not (regexp-match? #rx"/.git" (path->string p)))))

;; Return a list of directories only w/o git related dirs
(define (get-directories path)
  (filter valid-dir? (directory-list path #:build? #t)))

;; Sit on the result thread and print results
(define (print-result-thread result)
  (define done-count (count-done-threads result))
  (displayln (format "PRINT: ~a" result))
  (if (>= done-count WORKERS)
      'TERMINATE
      result))

;; Pull up the repo directories and feed to worker threads
(define (recurse-dirs state)
  (define repo-path (hash-ref (current-config) 'pwlrepo-path))
  (define directories
    (append (get-directories repo-path)
            (make-list WORKERS 'DONE)))

  ;; TODO: convert this to an interceptor for the DB channel
  (define result-thread
    (create-channel-sink result-channel print-result-thread))

  ;; Create worker threads to process paths
  (for ([i (range WORKERS)])
    (create-channel-interceptor work-channel result-channel worker-proc))

  ;; Push directory paths into work channel
  (for ([d directories])
    (channel-put work-channel (list d repo-path)))

  (for-each thread-wait (unbox work-threads))

  state)

;; Find README files in a dir and then recursively check for additional
;; folders to delve into
(define (handle-directory dir-path repo-path)
  (define dirs (get-directories dir-path))
  (define readme-path (build-path dir-path readme-md))
  (define metadata-path (build-path dir-path metadata-json))
  (define path-hash (hasheq 'dir-path dir-path 'repo-path repo-path 'dirs dirs))

  (define res
    (cond
      [(file-exists? metadata-path)
       (format "Metadata Found: ~a" metadata-path)]
      [(file-exists? readme-path)
       (format "Readme Found: ~a" (create-metadata-from-readme path-hash))]
      [else (format "Not Data File Found!: ~a" metadata-path)]))

  (define subs
    (for/list ([d dirs])
      (handle-directory d repo-path)))

  (cons res subs))

;; Pipeline README from markdown to metadata.json
(define (create-metadata-from-readme path-hash)
  (~> path-hash
      create-metadata-hash
      write-metadata-json))

;; Convert readme into a hash we can use for JSON
(define (create-metadata-hash paths)
  (define dir-path (hash-ref paths 'dir-path))
  (define repo-path (hash-ref paths 'repo-path))
  (define dirs (hash-ref paths 'dirs))
  (define relative-dir (find-relative-path repo-path dir-path))
  (define subs
    (map (λ (p) (path->string (find-relative-path repo-path p))) dirs))
  (define raw-links (call-with-input-file (build-path dir-path readme-md)
                      #:mode 'text get-readme-links))
  (define links (filter
                 not-false?
                 (for/list ([link raw-links])
                   (define url (last link))
                   (define title (first link))
                   (cond
                     [(or (scroll? title) (youtube? url)) #f]
                     [else (hash 'title title 'url url)]))))

  (hasheq 'directory dir-path
          'relative-directory (path->string relative-dir)
          'subdirectories subs
          'links links))

;; Write metadata hash to metadata.json file in directory
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

;; Consume a string port and extract Markdown links via regex
(define (get-readme-links in)
  (define text (port->string in))
  (regexp-match* re-markdown-link text
                 #:match-select cdr))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define (walk-dirs state)
  (guard-state recurse-dirs state))

;//////////////////////////////////////////////////////////////////////////////
; TESTS
