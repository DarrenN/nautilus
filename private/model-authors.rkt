#lang racket/base

(require db
         racket/match
         racket/dict
         "db-adapter.rkt"
         "parameters.rkt"
         "structs.rkt")

(provide insert-and-join-author)

#|

  Table schemas
  =============

  Authors
  -------

  (id integer primary key,
   name varchar unique not null,
   first_name varchar,
   middle_name varchar,
   last_name varchar)

  Authorpapers - join table for authors <--> papers
  -------------------------------------------------

  (id integer primary key,
   author_id integer not null,
   paper_id integer not null)

|#

;; Prepared queries
;; ================

(define/prepared-query db-insert-author
  "INSERT OR IGNORE INTO authors (name, first_name, middle_name, last_name)
   VALUES (?, ?, ?, ?)")

(define/prepared-query dp-select-author-by-name
  "SELECT id FROM authors WHERE name = ?")

(define/prepared-query db-insert-authorspapers
  "INSERT INTO authorspapers (paper_id, author_id) VALUES (?, ?)")

; If we didn't get an insert id back from the query, lets go find the record
(define (fetch-author-id logger conn author)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger author)])
    (define q (query-rows conn "select id from authors where name = $1"
                          (author-name author)))
    (if (not (null? q))
        (vector-ref (car q) 0)
        '())))

(define (insert-author-join logger conn author pid aid)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger author)])
    ;; check for existing joins
    (define join-rows
      (query-rows
       conn
       "SELECT paper_id, author_id FROM authorspapers
        WHERE paper_id = $1 AND author_id = $2"
       pid aid))

    ;; only insert joins if the join doesn't already exist
    (define q (if (null? join-rows)
                  (db-insert-authorspapers conn pid aid)
                  join-rows))

    (if (simple-result? q)
        (get-insert-id q)
        '())))

;; Use exceptions to handle dupe entries in DB and also situations where insert
;; ID wasn't returned from insert query.
;;
;; if insert fails then get author id and make join
;; otherwise make join with returned id
(define (insert-and-join-author logger conn author pid)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger author)])
    (define q (db-insert-author
               conn
               (author-name author)
               (author-first_name author)
               (author-middle_name author)
               (author-last_name author)))

    (define insert-id (if (simple-result? q)
                          (get-insert-id q)
                          '()))

    (define aid (if (null? insert-id)
                    (fetch-author-id logger conn author)
                    insert-id))

    (if (not (null? aid))
        (insert-author-join logger conn author pid aid)
        '())))
