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

Scratch queries
===============

; all papers with links and files

SELECT papers.title, '/' || files.directory || files.filename
FROM files
INNER JOIN papers ON papers.id = files.paper_id
UNION
SELECT papers.title, links.url
FROM links
INNER JOIN papers ON papers.id = links.paper_id
GROUP BY papers.title;

; papers with authors and tags

SELECT
    papers.title,
    group_concat(DISTINCT authors.name),
    group_concat(DISTINCT tags.tag)
FROM papers
JOIN authorspapers AS ap ON ap.paper_id = papers.id
JOIN authors ON authors.id = ap.author_id
JOIN tagspapers AS tp ON tp.paper_id = papers.id
JOIN tags ON tags.id = tp.tag_id
GROUP BY papers.title;

; with links/files information

SELECT
    papers.title,
    links.url,
    '/' || files.directory || files.filename,
    group_concat(DISTINCT authors.name),
    group_concat(DISTINCT tags.tag)
FROM papers
JOIN links ON links.paper_id = papers.id
JOIN files ON files.paper_id = papers.id
JOIN authorspapers AS ap ON ap.paper_id = papers.id
JOIN authors ON authors.id = ap.author_id
JOIN tagspapers AS tp ON tp.paper_id = papers.id
JOIN tags ON tags.id = tp.tag_id
GROUP BY papers.title;

; counts of papers to tag

SELECT
  tags.tag,
  count(papers.id)
FROM papers
JOIN tagspapers AS tp ON tp.paper_id = papers.id
JOIN tags ON tags.id = tp.tag_id
GROUP BY tags.tag;

; counts of papers to author

SELECT
  authors.name,
  count(papers.id)
FROM papers
JOIN authorspapers AS ap ON ap.paper_id = papers.id
JOIN authors ON authors.id = ap.author_id
GROUP BY authors.name;

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
   directory varchar not null,
   filename varchar not null,
   sha1 varchar unique not null,
   title varchar not null,
   created varchar,
   modified varchar);")

  (query-exec
   conn
   "CREATE TABLE IF NOT EXISTS links
  (id integer primary key,
   paper_id int,
   directory varchar not null,
   status int not null,
   title varchar not null,
   url varchar unique not null,
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
  "INSERT OR IGNORE INTO files
   (sha1, filename, directory, title, created, modified)
   values (?, ?, ?, ?, ?, ?)")

(define/prepared-query db-update-file-paper-id
  "UPDATE files SET paper_id = ? WHERE id = ?")

(define/prepared-query db-update-link-paper-id
  "UPDATE links SET paper_id = ? WHERE id = ?")

(define/prepared-query db-select-file-sha1
  "select sha1 from files where sha1 = ?")

(define/prepared-query db-insert-link
  "INSERT OR IGNORE INTO links
   (url, title, directory, status, created, modified)
   values (?, ?, ?, ?, ?, ?)")

;; Only pull entries where paper_id is NULL
(define (db-select-links conn)
  (query-rows conn "select * from links WHERE paper_id IS NULL ORDER BY directory"))

;; Only pull entries where paper_id is NULL
(define (db-select-files conn)
  (query-rows conn "select * from files WHERE paper_id is NULL ORDER BY directory"))

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
                 [(struct paper-response _)
                  (format "~a ~a" (paper-response-type ds)
                          (paper-response-id ds))]))

    (logger "~a" (format "SQLERROR ~a ~a for ~a ~a"
                         errcode message (object-name ds) id))

    'error))

; We attempt to insert everything, SQLite will throw an exception for duplicate
; sha1 fields which we mostly ignore
(define (insert-file logger conn file)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger file)])
    (db-insert-file conn
                    (pdf-directory file)
                    (pdf-filename file)
                    (pdf-sha1 file)
                    (pdf-title file)
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
    (db-update-file-paper-id conn pid (paper-response-id record))))

(define (update-link-paperid logger conn record pid)
  (with-handlers ([exn:fail:sql? (handle-sql-error logger record)])
    (db-update-link-paper-id conn pid (paper-response-id record))))

(define (get-insert-id q)
  (define info (simple-result-info q))
  (define insert-id (dict-ref info 'insert-id))
  (if (and (integer? insert-id)
           (positive? insert-id))
      insert-id
      '()))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit)

 (define mconn (sqlite3-connect #:database 'memory))
  (create-tables mconn)

  (test-pred "db-insert-file"
             simple-result?
             (db-insert-file mconn 0 1 2 3 4 5))

  (test-pred "db-update-file-paper-id"
             simple-result?
             (db-update-file-paper-id mconn 1 1))

  (test-pred "db-update-link-paper-id"
             simple-result?
             (db-update-link-paper-id mconn 1 1))

  (test-pred "db-select-file-sha1"
             rows-result?
             (db-select-file-sha1 mconn 0))

    (test-pred "db-insert-link"
             simple-result?
             (db-insert-link mconn 0 1 2 3 4 5))

    (test-pred "db-select-links"
             list?
             (db-select-links mconn))

    (test-pred "db-select-files"
             list?
             (db-select-files mconn))

  (define logger-call "")
  (define test-file (pdf 0 "kangaroo.pdf" "flooz" "frosty" 0 0))

  (define test-exn (exn:fail:sql
                    "fail:sql" (current-continuation-marks)
                    'constraint
                    (hash 'errcode 2067
                          'message "abort due to constraint violation")))

  (define (test-logger _ str)
    (set! logger-call str))

  ;; Call handle-sql-error from with-handlers
  (define (test-fail-handler)
    (with-handlers ([exn:fail:sql? (handle-sql-error test-logger test-file)])
      (query mconn "INSERT INTO files
                    (sha1, filename, directory, title, created, modified)
                    values (?, ?, ?, ?, ?, ?)"
             0 1 2 3 4 5)))

  (test-equal? "handle-sql-error calls logger and returns 'error on exn:fail:sql?"
               (test-fail-handler)
               'error)

  (test-equal?
   "handle-sql-error calls logger with string"
   logger-call
   "SQLERROR 2067 abort due to constraint violation for pdf flooz")

  ;; test handle-sql-error directly
  (test-case
      "handle-sql-error handles pdf structs"
    (check-equal? 'error ((handle-sql-error test-logger test-file) test-exn))
    (check-equal?
     logger-call
     "SQLERROR 2067 abort due to constraint violation for pdf flooz"))

  (test-case
      "handle-sql-error handles link structs"
    (define tl (link "badbrains.com" "i against i" "/tmp" 0 0 0))
    (check-equal? 'error ((handle-sql-error test-logger tl) test-exn))
    (check-equal?
     logger-call
     "SQLERROR 2067 abort due to constraint violation for link badbrains.com"))

    (test-case
        "handle-sql-error handles paper structs"
      (define tp (paper "reignition" 1986 "dc hardcore" "axiom" "/tmp" 0 0))
      (check-equal? 'error ((handle-sql-error test-logger tp) test-exn))
      (check-equal?
       logger-call
       "SQLERROR 2067 abort due to constraint violation for paper reignition"))

    (test-case
        "handle-sql-error handles author structs"
      (define ta (author "H. R." "H" "" "R"))
      (check-equal? 'error ((handle-sql-error test-logger ta) test-exn))
      (check-equal?
       logger-call
       "SQLERROR 2067 abort due to constraint violation for author H. R."))

    (test-case
        "handle-sql-error handles tag structs"
      (define tt (tag "dc-hardcore"))
      (check-equal? 'error ((handle-sql-error test-logger tt) test-exn))
      (check-equal?
       logger-call
       "SQLERROR 2067 abort due to constraint violation for tag dc-hardcore"))

    (test-case
        "handle-sql-error handles paper-response structs"
      (define tr (paper-response 1 'paper "Secret77" "yes"))
      (check-equal? 'error ((handle-sql-error test-logger tr) test-exn))
      (check-equal?
       logger-call
       "SQLERROR 2067 abort due to constraint violation for paper-response paper 1"))

    (test-case
      "insert-file returns 'ok on dupe insert"
      (check-equal?
       'ok
       (insert-file test-logger mconn
                    (pdf 0 "she's calling you" "/tmp" "" 0 0))))

    (test-case
        "insert-link returns 'ok on dupe insert"
      ;; create original link
      (define l (link "badbrains.com" "Hired Gun" "/tmp" 0 0 0))
      (insert-link test-logger mconn l)
      ;; test inserting a dupe
      (check-equal? 'ok (insert-link test-logger mconn l)))

    (test-case
        "get-insert-id returns insert-id if a positive integer"
      (check-equal? 1 (get-insert-id (simple-result (hash 'insert-id 1))))
      (check-equal? 101 (get-insert-id (simple-result (hash 'insert-id 101)))))

    (test-case
        "get-insert-id returns null if insert-id not a positive integer"
      (check-equal? null (get-insert-id (simple-result (hash 'insert-id #f))))
      (check-equal? null (get-insert-id (simple-result (hash 'insert-id -1))))))
