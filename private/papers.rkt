#lang racket/base

(require db
         racket/exn
         racket/match
         racket/string
         threading
         "db-adapter.rkt"
         "model-authors.rkt"
         "model-papers.rkt"
         "model-tags.rkt"
         "parameters.rkt"
         "semanticscholar.rkt"
         "structs.rkt"
         "utils.rkt")

(provide process-papers)

; (struct result-record (id record result) #:transparent)
; (struct paper (title year abstract venue directory created modified) #:transparent)

(define (update-source pr)
  (define-values (pid record) (get-values pr))
  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))

  (if (equal? (result-record-type record) 'link)
      (update-link-paperid logger conn record pid)
      (update-file-paperid logger conn record pid))

  (list pid record))

(define (update-authors pr)
  (define-values (pid record) (get-values pr))
  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define result (result-record-result record))
  (define structured-authors (hash-ref result 'structuredAuthors))
  (define authors (hash-ref result 'authors))

  ;; TODO: if no structure-authors then fall back on authors
  ;; NOTE: all name fields are run through string-titlecase

  (for ([sauthor structured-authors])
    (define-values (firstName middleName lastName)
      (match sauthor
        [(hash-table ('firstName a) ('middleNames b) ('lastName c))
         (values (string-titlecase a) (string-titlecase (string-join b))
                 (string-titlecase c))]))
    (define name (string-normalize-spaces
                  (string-join (list firstName middleName lastName))))
    (define a (author name firstName middleName lastName))
    (insert-and-join-author logger conn a pid))

  (list pid record))

(define (update-tags pr)
  (define-values (pid record) (get-values pr))
  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define tags (hash-ref (result-record-result record) 'tags))

  ;; NOTE: all tags should be lowercased!

  (for ([t tags])
    (insert-and-join-tag logger conn (tag (string-downcase t)) pid))

  (list pid record))

(define (pipeline-inserts pr)
  (~> pr
      update-source
      update-authors
      update-tags))

(define (insert-results record)
  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define datetime (timestamp))
  (define result (result-record-result record))
  (define rec (result-record-record record))
  (define directory
    (match rec
      [(struct link _) (link-directory rec)]
      [(struct pdf _) (pdf-directory rec)]))

  (define title (hash-ref result 'title))

  (define p (paper title (hash-ref result 'year) (hash-ref result 'abstract)
                   (hash-ref result 'venue) directory datetime datetime))

  (define paper-id (insert-paper logger conn p))
  ;; pass pid + record to update-source (update file/link with paper-id)
  ;; pass pid + record to update-authors (create authors/joins)
  ;; pass pid + record to update-tags (create tags/joins)
  (if paper-id
      (pipeline-inserts (list paper-id record))
      #f))

;; Take the db id and struct and determine what to query Semantic Scholar with
;; then hand off the result for insertion - the keyword optional args allow us
;; to mock this functions deps in tests
(define (fetch-and-insert-result
         id data-type
         #:fetch-with [fetch-semanticscholar fetch-semanticscholar]
         #:insert-with [insert-results insert-results]
         #:log-with [logger (hash-ref (current-config) 'logger)])
  (define query (match data-type
      [(struct link _) (link-title data-type)]
      [(struct pdf _) (pdf-normalized data-type)]))

  (define result (fetch-semanticscholar query))
  (if (and (list? result)
           (equal? (car result) 'ERROR))
      (begin
        (logger "~a" (format "FETCHERR (SEMANTIC) ~a" (cdr result)))
        result)
      (insert-results
       (result-record id (object-name data-type) data-type result))))

;; Destructure DB record into a link and dispatch
(define (process-link url vdetails)
  (define-values (id url title directory status created modified)
    (vector->values vdetails))
  (fetch-and-insert-result
   id (link url title directory status created modified)))

;; Destructure DB record into a pdf and dispatch
(define (process-file sha1 vdetails)
  (define-values (id filename directory normalized created modified)
    (vector->values vdetails))
  (fetch-and-insert-result
   id (pdf sha1 filename directory normalized created modified)))

(define (handle-papers state)
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define logger (hash-ref (current-config) 'logger))

  (with-handlers ([exn? (λ (e)
                          (logger "~a" (format "ERROR ~a\n" (exn->string e)))
                          (list 'error (exn->string)))])

    ;; TODO: These selects can be done first and then entries can be piped into
    ;; a buffered asynchronous channel that multiple threads can pull from to
    ;; make requests to Semantic Scholar:
    ;;
    ;; (for ([x xs])
    ;;  (async-channel-put ch (list 'link (hash-ref link-rows key))))

    (define link-rows
      (rows->dict
       (db-select-links conn)
       #:key "url"
       #:value '#("id" "url" "title" "status" "directory" "created" "modified")))

    (for ([key (hash-keys link-rows)])
      (process-link key (hash-ref link-rows key)))

    (define file-rows
      (rows->dict
       (db-select-files conn)
       #:key "sha1"
       #:value '#("id" "filename" "directory" "normalized" "created" "modified")))

    (for ([key (hash-keys file-rows)])
      (process-file key (hash-ref file-rows key)))

    (append state (list (format "Papers updated")))))

(define (process-papers state)
  (if (equal? (car state) 'error)
      state
      (handle-papers state)))

(module+ test
  (require rackunit
           racket/function
           mock
           mock/rackunit)

  (test-case "fetch-and-insert-result (link)"
    (define fetch-mock (mock #:behavior (const '(ok))))
    (define insert-mock (mock #:behavior (const '(12 null))))
    (define logger-mock (mock #:behavior void))
    (define tlink (link "http://pma.com" "PMA" "plt/" 0 0 0))

    ;; handle link
    (fetch-and-insert-result
     12 tlink
     #:fetch-with fetch-mock
     #:insert-with insert-mock
     #:log-with logger-mock)

    (check-mock-calls fetch-mock (list (arguments "PMA")))
    (check-mock-calls logger-mock null)
    (check-mock-calls
     insert-mock
     (list (arguments (result-record 12 'link tlink '(ok)))))

    (mock-reset! fetch-mock)
    (mock-reset! insert-mock)
    (mock-reset! logger-mock)

    (with-mock-behavior ([fetch-mock (const '(ERROR "nada"))])
      (fetch-and-insert-result
       12 tlink
       #:fetch-with fetch-mock
       #:insert-with insert-mock
       #:log-with logger-mock))

    (check-mock-calls fetch-mock (list (arguments "PMA")))
    (check-mock-calls logger-mock
                      (list (arguments "~a" "FETCHERR (SEMANTIC) (nada)")))
    (check-mock-calls insert-mock null))

  (test-case "process-papers let's 'error states fall through"
    (define err '(error ("Bad things went down")))
    (check-equal? err (process-papers err))))
