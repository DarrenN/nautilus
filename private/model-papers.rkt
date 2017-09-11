#lang racket/base

(require db
         racket/match
         racket/dict
         "db-adapter.rkt"
         "parameters.rkt"
         "structs.rkt")

(provide insert-paper)

#|

  Table schemas
  =============

  Papers
  -------

  (id integer primary key,
   title varchar unique not null,
   year int,
   abstract varchar,
   venue varchar,
   directory varchar,
   created varchar,
   modified varchar)

|#

;; Prepared queries
;; ================

(define/prepared-query db-update-link-paper-id
  "update links set paper_id = ? where id = ? ")

(define/prepared-query db-insert-paper
   "INSERT OR IGNORE INTO papers (title, year, abstract, venue, directory, created, modified) values (?, ?, ?, ?, ?, ?, ?)")

(define/prepared-query db-select-paper
   "select id from papers where title = ?")

(define (fetch-paper-id logger conn paper)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger paper)])
    (define q (query-rows conn "select id from papers where title = $1"
                          (paper-title paper)))
    (if (not (null? q))
        (vector-ref (car q) 0)
        '())))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

;; Insert or Ignore paper, and return paper id based on title select
(define (insert-paper logger conn paper)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger paper)])
    (define q1  (db-insert-paper
                 conn
                 (paper-title paper)
                 (paper-year paper)
                 (paper-abstract paper)
                 (paper-venue paper)
                 (paper-directory paper)
                 (paper-created paper)
                 (paper-modified paper)))

    (fetch-paper-id logger conn paper)))
