#lang racket/base

(require db
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

(define (process-link url vdetails)
  (define logger (hash-ref (current-config) 'logger))
  (define-values (id url title directory status created modified)
    (vector->values vdetails))

  (define result (fetch-semanticscholar title))
  (if (and (list? result)
           (equal? (car result) 'ERROR))
      (begin
        (logger "~a" (format "FETCHERR (SEMANTIC) ~a" (cdr result)))
        result)
      (insert-results
       (result-record id 'link (link url title directory status created modified)
                      result))))

(define (process-file sha1 vdetails)
  (define logger (hash-ref (current-config) 'logger))
  (define-values (id filename directory normalized created modified)
    (vector->values vdetails))
  (define result (fetch-semanticscholar normalized))
  (if (and (list? result)
           (equal? (car result) 'ERROR))
      (begin
        (logger "~a" (format "FETCHERR (SEMANTIC) ~a" (cdr result)))
        result)
      (insert-results
       (result-record
        id 'pdf (pdf sha1 filename directory normalized created modified) result))))

(define (handle-papers state)
  (define SQLITE-CONN (hash-ref (current-config) 'sqlite-conn))
  (define LOGGER (hash-ref (current-config) 'logger))

  ;; 1 zip over the Links in the DB and attempt to get from Semanticscholar
  ;;   1a. if paper and paper insert, update paper id in link table
  ;;   1b. if paper and paper insert, create author and tag associations
  ;; 2 zip over the Files in the DB and attempt to get from Semanticscholar
  ;;   2a. if paper and paper insert, update paper id in file table
  ;;   2b. if paper and paper insert, create author and tag associations

  (define link-rows
    (rows->dict
     (db-select-links SQLITE-CONN)
     #:key "url"
     #:value '#("id" "url" "title" "status" "directory" "created" "modified")))

  (for ([key (hash-keys link-rows)])
    (process-link key (hash-ref link-rows key)))

  (define file-rows
    (rows->dict
     (db-select-files SQLITE-CONN)
     #:key "sha1"
     #:value '#("id" "filename" "directory" "normalized" "created" "modified")))

  (for ([key (hash-keys file-rows)])
    (process-file key (hash-ref file-rows key)))

  state)

(define (process-papers state)
  (if (equal? (car state) 'error)
      state
      (handle-papers state)))
