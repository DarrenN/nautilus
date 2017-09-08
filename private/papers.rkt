#lang racket/base

(require db
         mock
         racket/exn
         racket/function
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

; (struct paper-response (id type record result) #:transparent)
; (struct paper (title year abstract venue directory created modified) #:transparent)

;; Update original link/file record with the paper id
(define/mock (update-source pr)
  #:opaque (test-connection test-logger)
  #:mock current-config #:as config-mock
  #:with-behavior (const (hash 'logger test-logger 'sqlite-conn test-connection))

  #:mock update-link-paperid #:as update-link-mock  #:with-behavior void
  #:mock update-file-paperid #:as update-file-mock  #:with-behavior void

  (define-values (pid paper-data) (get-values pr))
  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))

  (if (equal? (paper-response-type paper-data) 'link)
      (update-link-paperid logger conn paper-data pid)
      (update-file-paperid logger conn paper-data pid))

  (list pid paper-data))

;; Exract Author data from result and insert into DB
(define/mock (update-authors pr)
  #:opaque (test-connection test-logger)
  #:mock current-config #:as config-mock
  #:with-behavior (const (hash 'logger test-logger 'sqlite-conn test-connection))
  #:mock insert-and-join-author #:as insert-author-mock  #:with-behavior void

  (define-values (pid paper-data) (get-values pr))

  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define result (paper-response-result paper-data))

  ;; structured-authors and authors should be lists
  (define structured-authors (hash-ref result 'structuredAuthors '()))
  (define authors (hash-ref result 'authors '()))

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

  (list pid paper-data))

;; Extract tags and insert into DB
(define/mock (update-tags pr)
  #:opaque (test-connection test-logger)
  #:mock current-config #:as config-mock
  #:with-behavior (const (hash 'logger test-logger 'sqlite-conn test-connection))
  #:mock insert-and-join-tag #:as insert-tag-mock  #:with-behavior void

  (define-values (pid paper-data) (get-values pr))
  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define tags (hash-ref (paper-response-result paper-data) 'tags))

  ;; NOTE: all tags should be lowercased!
  (for ([t tags])
    (insert-and-join-tag logger conn (tag (string-downcase t)) pid))

  (list pid paper-data))

(define (pipeline-inserts pr)
  (~> pr
      update-source
      update-authors
      update-tags))

(define (insert-results record)
  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define datetime (timestamp))
  (define result (paper-response-result record))
  (define rec (paper-response-record record))
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
(define/mock (fetch-and-insert-result
              id data-type [logger (hash-ref (current-config) 'logger)])

  #:mock fetch-semanticscholar #:as fetch-mock #:with-behavior (const '(ok))
  #:mock insert-results #:as insert-mock #:with-behavior (const '(12 null))

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
       (paper-response id (object-name data-type) data-type result))))

;; Destructure DB record into a link and dispatch
(define/mock (process-link url vdetails)
  #:mock fetch-and-insert-result #:as fetch-result-mock  #:with-behavior void

  (define-values (id url title directory status created modified)
    (vector->values vdetails))
  (fetch-and-insert-result
   id (link url title directory status created modified)))

;; Destructure DB record into a pdf and dispatch
(define/mock (process-file sha1 vdetails)
  #:mock fetch-and-insert-result #:as fetch-result-mock  #:with-behavior void

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

  (test-case "update-tags"
    (define pid 128)
    (define res
      (hasheq 'tags (list "CON-SORDINO" "mOuNtAiN" "Small Axe"))))

  (test-case "update-authors"
    (define pid 64)
    (define res
      (hasheq 'structuredAuthors (list (hasheq 'firstName "MOLLY"
                                               'middleNames '("b.")
                                               'lastName "rankin"))))
    (define record (paper-response 32 'link '() res))

    (with-mocks update-authors
      (define r (update-authors (list pid record)))
      (check-equal? r (list pid record))
      (check-mock-calls
       insert-author-mock
       (list (arguments test-logger test-connection
                        (author "Molly B. Rankin" "Molly"
                                "B." "Rankin") pid)))
      (check-equal? r (list pid record))))

  (test-case "update-source"
    (define linkr (paper-response 12 'link '() '()))
    (define pdfr (paper-response 12 'pdf '() '()))

    (with-mocks update-source
      (define r (update-source (list 36 linkr)))
      (check-equal? r (list 36 linkr))
      (check-mock-num-calls update-file-mock 0)
      (check-mock-calls
       update-link-mock
       (list (arguments test-logger test-connection linkr 36))))

    (with-mocks update-source
      (define r (update-source (list 36 pdfr)))
      (check-equal? r (list 36 pdfr))
      (check-mock-num-calls update-link-mock 0)
      (check-mock-calls
       update-file-mock
       (list (arguments test-logger test-connection pdfr 36)))))

  (test-case "process-link"
    (define vdetails
      (vector 12 "http://deerhoof.com" "Mountain Moves" "deerhoof/" 0 0 0))

    (with-mocks process-link
      (with-mock-behavior ([fetch-result-mock (const '(12 null))])
        (process-link "http://deerhood.com" vdetails)
        (check-mock-calls
         fetch-result-mock
         (list (arguments 12 (link "http://deerhoof.com" "Mountain Moves"
                                   "deerhoof/" 0 0 0)))))))

  (test-case "process-file"
    (define vdetails
      (vector 12 "deerhoof.pdf" "deerhoof/" "Deerhoof" 0 0))

    (with-mocks process-file
      (with-mock-behavior ([fetch-result-mock (const '(12 null))])
        (process-file "123abc" vdetails)
        (check-mock-calls
         fetch-result-mock
         (list (arguments 12 (pdf "123abc" "deerhoof.pdf" "deerhoof/"
                                  "Deerhoof" 0 0)))))))

  (test-case "fetch-and-insert-result (link)"
    (define logger-mock (mock #:behavior void))
    (define tlink (link "http://pma.com" "PMA" "plt/" 0 0 0))
    (define tpdf (pdf "123abc" "deerhoof.pdf" "deerhoof/" "Mountain Moves" 0 0))

    ;; handle link
    (with-mocks fetch-and-insert-result
      (fetch-and-insert-result 12 tlink logger-mock)

      (check-mock-calls fetch-mock (list (arguments "PMA")))
      (check-mock-calls logger-mock null)
      (check-mock-calls
       insert-mock
       (list (arguments (paper-response 12 'link tlink '(ok)))))
      (mock-reset! logger-mock))

    ;; handle link with fetch error
    (with-mocks fetch-and-insert-result
      (with-mock-behavior ([fetch-mock (const '(ERROR "nada"))])
        (fetch-and-insert-result 12 tlink logger-mock))

      (check-mock-calls fetch-mock (list (arguments "PMA")))
      (check-mock-calls logger-mock
                        (list (arguments "~a" "FETCHERR (SEMANTIC) (nada)")))
      (check-mock-calls insert-mock null)
      (mock-reset! logger-mock))

    ;; handle pdf
    (with-mocks fetch-and-insert-result
      (fetch-and-insert-result 12 tpdf logger-mock)

      (check-mock-calls fetch-mock (list (arguments "Mountain Moves")))
      (check-mock-calls logger-mock null)
      (check-mock-calls
       insert-mock
       (list (arguments (paper-response 12 'pdf tpdf '(ok)))))
      (mock-reset! logger-mock))

    ;; handle pdf with fetch error
    (with-mocks fetch-and-insert-result
      (with-mock-behavior ([fetch-mock (const '(ERROR "nada"))])
        (fetch-and-insert-result 12 tpdf logger-mock))

      (check-mock-calls fetch-mock (list (arguments "Mountain Moves")))
      (check-mock-calls logger-mock
                        (list (arguments "~a" "FETCHERR (SEMANTIC) (nada)")))
      (check-mock-calls insert-mock null)
      (mock-reset! logger-mock)))

  (test-case "process-papers let's 'error states fall through"
    (define err '(error ("Bad things went down")))
    (check-equal? err (process-papers err))))
