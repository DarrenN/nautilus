#lang racket/base

(require db
         gregor
         racket/match
         threading
         "db-adapter.rkt"
         "parameters.rkt"
         "semanticscholar.rkt"
         "structs.rkt")

(provide process-papers)

(struct link-paper (url id title status directory result) #:transparent)
(struct file-paper (sha1 id filename normalized directory result) #:transparent)

; (struct paper (title year abstract venue directory created modified) #:transparent)

(define (timestamp)
  (datetime->iso8601 (now/utc)))

(define (get-values xs)
  (match xs
    [(list a b) (values a b)]))

(define (update-source pr)
  (define-values (pid record) (get-values pr))
  (printf "update-source: ~a ~a\n" pid record)
  (list pid record))

(define (update-authors pr)
  (define-values (pid record) (get-values pr))
  (printf "update-authors: ~a ~a\n" pid record)
  (list pid record))

(define (update-tags pr)
  (define-values (pid record) (get-values pr))
  (printf "update-tags: ~a ~a\n" pid record)
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

  (define-values (directory result)
    (match record
      [(struct link-paper _)
       (values (link-paper-directory record) (link-paper-result record))]
      [(struct file-paper _)
       (values (file-paper-directory record) (file-paper-result record))]))

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
  (define-values (id title status directory) (vector->values vdetails))
  (define result (fetch-semanticscholar title))
  (if (and (list? result)
           (equal? (car result) 'ERROR))
      (begin
        (logger "~a" (format "FETCHERR (SEMANTIC) ~a" (cdr result)))
        result)
      (insert-results (link-paper url id title status directory result))))

(define (process-file url vdetails)
  (define logger (hash-ref (current-config) 'logger))
  (define-values (sha1 id filename normalized directory)
    (vector->values vdetails))
  (define result (fetch-semanticscholar normalized))
  (if (and (list? result)
           (equal? (car result) 'ERROR))
      (begin
        (logger "~a" (format "FETCHERR (SEMANTIC) ~a" (cdr result)))
        result)
      (insert-results
       (file-paper sha1 id filename normalized directory result))))

(define (handle-papers state)
  (define SQLITE-CONN (hash-ref (current-config) 'sqlite-conn))
  (define LOGGER (hash-ref (current-config) 'logger))

  ;; 1 zip over the Links in the DB and attempt to get from Semanticscholar
  ;;   1a. if paper and paper insert, update paper id in link table
  ;;   1b. if paper and paper insert, create author and tag associations
  ;; 2 zip over the Files in the DB and attempt to get from Semanticscholar
  ;;   2a. if paper and paper insert, update paper id in file table
  ;;   2b. if paper and paper insert, create author and tag associations

  (define link-rows (rows->dict (db-select-links SQLITE-CONN)
                                #:key "url"
                                #:value '#("id" "title" "status" "directory")))

  (define file-rows (db-select-files SQLITE-CONN))

  (define f (car (hash-keys link-rows)))

  (println (process-link f (hash-ref link-rows f)))
  state)

(define (process-papers state)
  (if (equal? (car state) 'error)
      state
      (handle-papers state)))
