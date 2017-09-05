#lang racket/base

(require db
         racket/match
         racket/dict
         "db-adapter.rkt"
         "parameters.rkt"
         "structs.rkt")

(provide insert-and-join-tag)

#|

  Table schemas
  =============

  Tags
  ----

  (id integer primary key,
   tag varchar unique not null)

  Tagspapers - join table for tags <--> papers
  --------------------------------------------

  (id integer primary key,
   tag_id integer not null,
   paper_id integer not null)

|#

;; Prepared queries
;; ================

(define/prepared-query db-insert-tag
  "INSERT OR IGNORE INTO tags (tag) VALUES (?)")

(define/prepared-query db-insert-tagpapers
   "INSERT INTO tagspapers (tag_id, paper_id) VALUES (?, ?)")

(define (fetch-tag-id logger conn tag)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger tag)])
    (define q (query-rows
               conn
               "SELECT id FROM tags WHERE tag = $1"
                          (tag-tag tag)))
    (if (not (null? q))
        (vector-ref (car q) 0)
        '())))

(define (insert-tag-join logger conn tag pid tid)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger tag)])
    ;; check for existing joins
    (define join-rows
      (query-rows
       conn
       "SELECT paper_id, tag_id FROM tagspapers
        WHERE paper_id = $1 AND tag_id = $2"
       pid tid))

    (define q (if (null? join-rows)
                  (db-insert-tagpapers conn tid pid)
                  join-rows))

    (if (simple-result? q)
        (get-insert-id q)
        '())))

(define (insert-and-join-tag logger conn tag pid)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger tag)])
    (define q (db-insert-tag conn (tag-tag tag)))
    (define tid (fetch-tag-id logger conn tag))

    (if (not (null? tid))
        (insert-tag-join logger conn tag pid tid)
        '())))
