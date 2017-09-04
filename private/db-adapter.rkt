#lang racket/base

(require db
         racket/dict
         racket/match
         racket/function
         (for-syntax racket/base
                     racket/syntax
                     db)
         "parameters.rkt"
         "structs.rkt")

(provide (all-defined-out))

#|
File schema:
------------
id
paper_id
sha1
directory
filename
normalized
created ISO8601
modified ISO8601

Link schema: (links from README and from data sources)
------------
id
paper_id
url
directory
status <int>
created ISO8601
modified ISO8601

Paper schema:
-------------
id
title
year
abstract
venue
directory
created ISO8601
modified ISO8601

Author schema:
--------------
id
paper_id
name
firstName
middleName
lastName

Tag schema:
-----------
id
tag unique

TagsPapers schema:
------------------
id
paper_id
tag_id

|#

(define DEFAULT-DB "nautilus-test.db")

(define (create-connection [dbname DEFAULT-DB])
  (sqlite3-connect #:database dbname
                   #:mode 'create))

(define (create-tables conn)
  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS papers
  (id integer primary key,
   title varchar unique not null,
   year int,
   abstract varchar,
   venue varchar,
   directory varchar,
   created varchar,
   modified varchar);")

  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS authors
  (id integer primary key,
   name varchar unique not null,
   first_name varchar,
   middle_name varchar,
   last_name varchar);")

  ; join table for tags <--> papers
  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS authorspapers
  (id integer primary key,
   author_id integer not null,
   paper_id integer not null);")

  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS tags
  (id integer primary key,
   tag varchar unique not null);")

  ; join table for tags <--> papers
  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS tagspapers
  (id integer primary key,
   tag_id integer not null,
   paper_id integer not null);")

  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS files
  (id integer primary key,
   paper_id int,
   sha1 varchar unique not null,
   filename varchar not null,
   directory varchar not null,
   normalized varchar not null,
   created varchar,
   modified varchar);")

  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS links
  (id integer primary key,
   paper_id int,
   url varchar unique not null,
   title varchar not null,
   directory varchar not null,
   status int not null,
   created varchar,
   modified varchar);"))

(define-syntax (define/prepared-query stx)
  (syntax-case stx ()
    [(_ name sql)
     #'(define (name conn . values)
         (apply (curry query conn (prepare conn sql)) values))]))

; (define/prepared-query name sql) -> (name conn values...)
; (define/prepared-query insert-tag ...) -> (insert-tag "foo")

(define/prepared-query db-insert-file
  "insert into files (sha1, filename, directory, normalized, created, modified) values (?, ?, ?, ?, ?, ?)")

(define/prepared-query db-update-file-paper-id
  "update files set paper_id = ? where id = ? ")

(define/prepared-query db-select-file-sha1
  "select sha1 from files where sha1 = ?")

(define/prepared-query db-insert-link
  "insert into links (url, title, directory, status, created, modified) values (?, ?, ?, ?, ?, ?)")

(define/prepared-query db-update-link-paper-id
  "update links set paper_id = ? where id = ? ")

(define (db-select-links conn)
  (query conn "select * from links WHERE paper_id IS NULL ORDER BY directory"))

(define (db-select-files conn)
  (query conn "select * from files WHERE paper_id is NULL ORDER BY directory"))

(define/prepared-query db-insert-paper
   "insert into papers (title, year, abstract, venue, directory, created, modified) values (?, ?, ?, ?, ?, ?, ?)")

(define/prepared-query db-select-paper
   "select id from papers where title = ?")

(define/prepared-query db-insert-author
  "insert into authors (name, first_name, middle_name, last_name) values (?, ?, ?, ?)")

(define/prepared-query dp-select-author-by-name
  "select id from authors where name = ?")

(define/prepared-query db-insert-authorspapers
  "insert into authorspapers (paper_id, author_id) values (?, ?)")

(define/prepared-query db-insert-tag
   "insert into tags (tag) values (?)")

(define/prepared-query db-insert-tagpaper
   "insert into tagspapers (tag_id, paper_id) values (?, ?)")

;; If the exception isn't due to a dupe unique val then we need to log the error
(define (handle-sql-error logger ds)
  (λ (e)
    (define err (exn:fail:sql-info e))
    (define errcode (dict-ref err 'errcode))
    (define message (dict-ref err 'message))
    (define id (match ds
                 [(struct pdf _) (pdf-filename ds)]
                 [(struct link _) (link-url ds)]
                 [(struct author _) (author-name ds)]
                 [(struct paper _) (paper-title ds)]))

    (if (not (equal? errcode 2067)) ; 2067 constraint violation (duplicate)
        (begin
          (logger
           "~a"
           (format "SQLERROR ~a ~a for PDF ~a" errcode message id))
          'fatal)
        'ok)))

; We attempt to insert everything, SQLite will throw an exception for duplicate
; sha1 fields which we mostly ignore
(define (insert-file logger conn file)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger file)])
    (db-insert-file conn
                    (pdf-sha1 file)
                    (pdf-filename file)
                    (pdf-directory file)
                    (pdf-normalized file)
                    (pdf-created file)
                    (pdf-modified file))
    'ok))

(define (insert-link logger conn link)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger link)])
    (db-insert-link conn
                    (link-url link)
                    (link-title link)
                    (link-directory link)
                    (link-status link)
                    (link-created link)
                    (link-modified link))
    'ok))

(define (get-insert-id q)
  (define info (simple-result-info q))
  (define insert-id (dict-ref info 'insert-id))
  (if (and (integer? insert-id)
           (positive? insert-id))
      insert-id
      '()))

(define (fetch-paper-id logger conn paper)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger paper)])
    (define q (query-rows conn "select id from papers where title = $1"
                          (paper-title paper)))
    (if (not (null? q))
        (vector-ref (car q) 0)
        '())))

;; If the papers exists return the paper's id
(define (handle-dupe-paper logger conn paper)
  (λ (e)
    (define err (exn:fail:sql-info e))
    (define errcode (dict-ref err 'errcode))
    (define message (dict-ref err 'message))
    (define title (paper-title paper))

    (if (not (equal? errcode 2067)) ; 2067 constraint violation (duplicate)
        (begin
          (logger
           "~a"
           (format "SQLERROR ~a ~a for PAPER ~a" errcode message title))
          'fatal)
        (fetch-paper-id logger conn paper))))

(define (insert-paper logger conn paper)
  (with-handlers ([exn:fail:sql? (handle-dupe-paper logger conn paper)])
    (define q  (db-insert-paper
                conn
                (paper-title paper)
                (paper-year paper)
                (paper-abstract paper)
                (paper-venue paper)
                (paper-directory paper)
                (paper-created paper)
                (paper-modified paper)))
    (if (simple-result? q)
        (get-insert-id q)
        '())))

;; If the exception isn't due to a dupe sha1 then we need to log the error
(define (handle-unique-author logger conn author pid)
  (λ (e)
    (define err (exn:fail:sql-info e))
    (define errcode (dict-ref err 'errcode))
    (define message (dict-ref err 'message))
    (define id (author-name author))
    (define aid (fetch-author-id logger conn author))

    (if (not (equal? errcode 2067)) ; 2067 constraint violation (duplicate)
        (begin
          (logger
           "~a"
           (format "SQLERROR ~a ~a for AUTHOR ~a" errcode message id))
          'fatal)
        (begin
          (if (not (null? aid))
              (insert-author-join logger conn author pid aid)
              (begin
                (logger
                 "~a"
                 (format "SQLERROR: NOT EXISTS ~a ~a for AUTHOR ~a"
                         errcode message id))
                'fatal))))))

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
       "select paper_id, author_id from authorspapers where paper_id = $1 and author_id = $2"
       pid aid))

    ;; only insert joins if the join doesn't already exist
    (define q (if (null? join-rows)
                  (db-insert-authorspapers conn pid aid)
                  join-rows))

    (if (simple-result? q)
        (get-insert-id q)
        '())))

;; if insert fails then get author id and make join
;; otherwise make join with returned id
(define (insert-and-join-author logger conn author pid)
  (with-handlers ([exn:fail:sql? (handle-unique-author logger conn author pid)])
    ;"insert into authors (paper_id, name, first_name, middle_name, last_name)"
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

    (if (simple-result? q)
        (insert-author-join logger conn author pid aid)
        '())))
