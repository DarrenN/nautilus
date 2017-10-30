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

  #:mock update-link-paperid #:as update-link-mock #:with-behavior void
  #:mock update-file-paperid #:as update-file-mock #:with-behavior void

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

;; If paper-data isn't a paper ressponse, its prolly an error and send
;; it off to the response-chan
(define (guard-insert paper-data)
  (if (paper-response? paper-data)
      (insert-results paper-data)
      paper-data))

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

  (if paper-id
      (pipeline-inserts (list paper-id paper-data))
      #f))

(define (create-link row)
  (define-values (id paper_id url title directory status created modified)
    (vector->values row))
  (values id title (link url title directory status created modified)))

(define (create-pdf row)
  (define-values (id paper_id sha1 filename directory normalized created
                     modified)
    (vector->values row))
  (values id normalized (pdf sha1 filename directory normalized created
                             modified)))

(define/mock (fetch-metadata item)
  #:opaque (test-logger)
  #:mock current-config #:as config-mock
  #:with-behavior (const (hash 'logger test-logger))
  #:mock fetch-semanticscholar #:as fetch-mock #:with-behavior (const '(ok))

  (define logger (hash-ref (current-config) 'logger))
  (define type (car item))
  (define row (cadr item))

  (define-values (id query data-type)
    (case type
      [(link) (create-link row)]
      [(file) (create-pdf row)]))

  (define result (fetch-semanticscholar query))
  (if (and (list? result)
           (equal? (car result) 'ERROR))
      (begin
        (logger "~a" (format "FETCHERR (SEMANTIC) ~a" (cdr result)))
        result)
      (paper-response id (object-name data-type) data-type result)))

(define WEB-WORKER-COUNT 10)
(define DB-WORKER-COUNT 1)

(define web-chan (make-async-channel))
(define db-chan (make-async-channel))
(define result-chan (make-async-channel))

(define (web-worker id input-channel output-channel)
  (define th
    (thread (λ ()
              (define (get)
                (async-channel-get input-channel))
              (define (put x)
                (async-channel-put output-channel x))
              (let loop ([data (get)])
                (case data
                  [(quit) (begin
                            (increment-web-worker-state)
                            (put data)
                            (kill-thread th))]
                  [else (begin
                          (put (fetch-metadata data))
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
                (case data
                  [(quit) (if (equal? (unbox web-worker-state) WEB-WORKER-COUNT)
                              (begin
                                (increment-db-worker-state)
                                (put data)
                                (kill-thread th))
                              (loop (get)))]
                  [else (begin
                          (put (guard-insert data))
                          (loop (get)))])))))
  th)

(define web-worker-state (box 0))
(define db-worker-state (box 0))

(define web-workers '())
(define db-workers '())

(define (increment-web-worker-state)
  (set-box! web-worker-state (add1 (unbox web-worker-state))))

(define (increment-db-worker-state)
  (set-box! db-worker-state (add1 (unbox db-worker-state))))

(define (handle-papers state)
  (define conn (hash-ref (current-config) 'sqlite-conn))
  (define logger (hash-ref (current-config) 'logger))

  (with-handlers ([exn? (λ (e)
                          (logger "~a" (format "ERROR ~a\n" (exn->string e)))
                          (list 'error (exn->string e)))])

    ;; IMPORTANT: We need to setup the worker threads here so they have the
    ;; correct parameterization of current-config
    (set! web-workers (map (λ (id) (web-worker id web-chan db-chan))
                           (range WEB-WORKER-COUNT)))

    (set! db-workers (map (λ (id) (db-worker id db-chan result-chan))
                          (range DB-WORKER-COUNT)))

    (define link-rows (db-select-links conn))
    (define file-rows (db-select-files conn))

    (for ([link-row link-rows])
      (async-channel-put web-chan (list 'link link-row)))

    (for ([file-row file-rows])
      (async-channel-put web-chan (list 'file file-row)))

    ;; Send kill signals to web-workers, these cascade through the
    ;; channels eventually terminating the result-worker below
    (for ([q (make-list WEB-WORKER-COUNT 'quit)])
      (async-channel-put web-chan q))

    ;; Block on the result-channel until all the db workers are done
    (define result
      (let loop ([data (async-channel-get result-chan)])
        (case data
          [(quit) (if (equal? (unbox db-worker-state) DB-WORKER-COUNT)
                      "Papers updated"
                      (loop (async-channel-get result-chan)))]
          [else (loop (async-channel-get result-chan))])))
    (append state (list (format "Process papers: ~a" result)))))

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

  (test-case "fetch-metadata"
    (define l (vector 1 1 "http://oolong.com" "Oolong" "oolong/" 0 0 0))

    (with-mocks fetch-metadata
      (define resp (fetch-metadata (list 'link l)))
      (check-pred paper-response? resp)
      (check-equal? (paper-response-id resp) 1)
      (check-equal? (paper-response-type resp) 'link)
      (check-pred link? (paper-response-record resp))
      (check-mock-calls fetch-mock
                        (list (arguments "Oolong")))))

  (test-case "create-link"
    (define l (vector 1 1 "http://oolong.com" "Oolong" "oolong/" 0 0 0))
    (define-values (id query data-type) (create-link l))
    (check-pred link? data-type)
    (check-equal? 1 id)
    (check-equal? "Oolong" query))

  (test-case "create-pdf"
    (define l (vector 1 1 "123abc" "oolong.pdf" "oolong/" "Oolong" 0 0))
    (define-values (id query data-type) (create-pdf l))
    (check-pred pdf? data-type)
    (check-equal? 1 id)
    (check-equal? "Oolong" query))

  (test-case "insert-results"
    ; (struct paper-response (id type record result) #:transparent)
    (define record (pdf "123abc" "Deerhoof" "deerhoof.pdf" "deerhoof/" 0 0))
    (define result (hasheq 'title "Kafe Mania!"
                           'year 2016
                           'abstract "The Magic is a record!"
                           'venue "Villain"))
    (define paper-data (paper-response 16 'pdf record result))
    (define pstruct (paper "Kafe Mania!" 2016 "The Magic is a record!" "Villain"
                           "deerhoof/" 0 0))

    ;; called with paper-response
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

  (test-case "process-papers let's 'error states fall through"
    (define err '(error ("Bad things went down")))
    (check-equal? err (process-papers err))))
