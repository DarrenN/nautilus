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
  "INSERT OR IGNORE INTO files (sha1, filename, directory, normalized, created, modified) values (?, ?, ?, ?, ?, ?)")

(define/prepared-query db-update-file-paper-id
  "UPDATE files SET paper_id = ? WHERE id = ?")

(define/prepared-query db-update-link-paper-id
  "UPDATE links SET paper_id = ? WHERE id = ?")

(define/prepared-query db-select-file-sha1
  "select sha1 from files where sha1 = ?")

(define/prepared-query db-insert-link
  "INSERT OR IGNORE INTO links (url, title, directory, status, created, modified) values (?, ?, ?, ?, ?, ?)")

;; Only pull entries where paper_id is NULL
(define (db-select-links conn)
  (query conn "select * from links WHERE paper_id IS NULL ORDER BY directory"))

;; Only pull entries where paper_id is NULL
(define (db-select-files conn)
  (query conn "select * from files WHERE paper_id is NULL ORDER BY directory"))

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
                 [(struct paper _) (paper-title ds)]
                 [(struct tag _) (tag-tag ds)]
                 [(struct result-record _)
                  (format "~a ~a" (result-record-type ds)
                          (result-record-id ds))]))

    (logger "~a" (format "SQLERROR ~a ~a for ~a ~a"
                         errcode message (object-name ds) id))

    'fatal))

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

(define (update-file-paperid logger conn record pid)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger record)])
    (db-update-file-paper-id conn pid (result-record-id record))))

(define (update-link-paperid logger conn record pid)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger record)])
    (db-update-link-paper-id conn pid (result-record-id record))))

(define (get-insert-id q)
  (define info (simple-result-info q))
  (define insert-id (dict-ref info 'insert-id))
  (if (and (integer? insert-id)
           (positive? insert-id))
      insert-id
      '()))
