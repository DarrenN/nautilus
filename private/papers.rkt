#lang racket/base

(require db
         mock
         racket/async-channel
         racket/exn
         racket/function
         racket/list
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

(define/mock (insert-results paper-data)
  #:opaque (test-connection test-logger)
  #:mock current-config #:as config-mock
  #:with-behavior (const (hash 'logger test-logger 'sqlite-conn test-connection))
  #:mock insert-paper #:as insert-mock  #:with-behavior void
  #:mock pipeline-inserts #:as pipeline-mock  #:with-behavior void
  #:mock timestamp #:as timestamp-mock  #:with-behavior (const 0)

  (define logger (hash-ref (current-config) 'logger))
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define datetime (timestamp))
  (define result (paper-response-result paper-data))
  (define rec (paper-response-record paper-data))
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
      (pipeline-inserts (list paper-id paper-data))
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

(define WEB-WORKER-COUNT 10)
(define DB-WORKER-COUNT 2)

(define web-chan (make-async-channel))
(define db-chan (make-async-channel (* WEB-WORKER-COUNT 2)))
(define result-chan (make-async-channel))

(define (web-worker id input-channel output-channel)
  (define th
    (thread (λ ()
              (define (get)
                (async-channel-get input-channel))
              (define (put x)
                (async-channel-put output-channel x))
              (let loop ([data (get)])
                (printf "web-worker ~a data: ~a\n" id data)
                (case data
                  [(quit) (begin (put data) (kill-thread th))]
                  [else (begin
                          (put data)
                          (loop (get)))])))))
  th)

(define (db-worker id input-channel output-channel)
  (define th
    (thread (λ ()
              (define (get)
                (async-channel-get input-channel))
              (define (put x)
                (async-channel-put output-channel x))
              (let loop ([data (get)])
                (printf "db-worker ~a data: ~a\n" id data)
                (case data
                  [(quit) (if (web-workers-dead?)
                              (begin (put data) (kill-thread th))
                              (loop (get)))]
                  [else (begin
                          (put (list 'ok id))
                          (loop (get)))])))))
  th)

(define web-workers (map (λ (id) (web-worker id web-chan db-chan))
                         (range WEB-WORKER-COUNT)))

(define db-workers (map (λ (id) (db-worker id db-chan result-chan))
                        (range DB-WORKER-COUNT)))

(define (web-workers-dead?)
  (not (ormap thread-running? web-workers)))

(define (db-workers-dead?)
  (not (ormap thread-running? db-workers)))

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
       #:value '#("id" "url" "title" "status" "directory" "created"
                       "modified")))

    (for ([key (hash-keys link-rows)])
      (process-link key (hash-ref link-rows key))
      ;(async-channel-put web-chan (hash-ref link-rows key))
      ))

    (define file-rows
      (rows->dict
       (db-select-files conn)
       #:key "sha1"
       #:value '#("id" "filename" "directory" "normalized" "created"
                       "modified")))

  #|
    (for ([key (hash-keys file-rows)])
      (process-file key (hash-ref file-rows key)))


  (define output
      (let loop ([data (async-channel-get result-chan)])
        (printf "result-chan ~a\n")
        (case data
          [(quit) (if (db-workers-dead?)
                      'all-done
                      (loop (async-channel-get result-chan)))]
          [else (loop (async-channel-get result-chan))])))

    (printf "OUTPUT ~a\n" output)
|#
    (append state (list (format "Papers updated"))))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define (process-papers state)
  (if (equal? (car state) 'error)
      state
      (handle-papers state)))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit
           racket/function
           mock
           mock/rackunit)

  (test-case "insert-results"
    ; (struct paper-response (id type record result) #:transparent)
    (define record (pdf "123abc" "deerhoof.pdf" "deerhoof/" "Deerhoof" 0 0))
    (define result (hasheq 'title "Kafe Mania!"
                           'year 2016
                           'abstract "The Magic is a record!"
                           'venue "Villain"))
    (define paper-data (paper-response 16 'pdf record result))
    (define pstruct (paper "Kafe Mania!" 2016 "The Magic is a record!" "Villain"
                           "deerhoof/" 0 0))

    (with-mocks insert-results
      (with-mock-behavior ([insert-mock (const 16)]
                           [pipeline-mock (const (list 16 paper-data))])
        (define r (insert-results paper-data))
        (check-equal? r (list 16 paper-data))
        (check-mock-calls insert-mock
                          (list (arguments test-logger test-connection
                                           pstruct)))
        (check-mock-calls pipeline-mock
                          (list (arguments (list 16 paper-data)))))))

  (test-case "update-tags"
    (define pid 128)
    (define res
      (hasheq 'tags (list "CON-SORDINO" "mOuNtAiN" "Small Axe")))
    (define paper-data (paper-response 128 'link '() res))

    (with-mocks update-tags
      (define r (update-tags (list pid paper-data)))
      (check-equal? r (list pid paper-data))
      (check-mock-calls
       insert-tag-mock
       (list (arguments test-logger test-connection (tag "con-sordino") pid)
             (arguments test-logger test-connection (tag "mountain") pid)
             (arguments test-logger test-connection (tag "small axe") pid)))))

  (test-case "update-authors"
    (define pid 64)
    (define res
      (hasheq 'structuredAuthors (list (hasheq 'firstName "MOLLY"
                                               'middleNames '("b.")
                                               'lastName "rankin")
                                       (hasheq 'firstName "Satomi"
                                               'middleNames '()
                                               'lastName "MATSUZAKI"))))

    (define record (paper-response 32 'link '() res))

    (with-mocks update-authors
      (define r (update-authors (list pid record)))
      (check-equal? r (list pid record))
      (check-mock-calls
       insert-author-mock
       (list (arguments test-logger test-connection
                        (author "Molly B. Rankin" "Molly"
                                "B." "Rankin") pid)
             (arguments test-logger test-connection
                        (author "Satomi Matsuzaki" "Satomi"
                                "" "Matsuzaki") pid)))
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
