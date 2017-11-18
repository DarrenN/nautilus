#lang racket/base

(require db
         threading
         "db-adapter.rkt"
         "fs.rkt"
         "git.rkt"
         "logger.rkt"
         "papers.rkt"
         "parameters.rkt"
         "readmes.rkt"
         "threads.rkt")

(provide nautilus-go)

;; Mutable state
(define logging-thread '())

;; Logging
;; =======
(define (create-logging-thread path)
  (set! logging-thread
        (launch-log-daemon (path->complete-path path) "nautilus")))

;; Iterate over result messages list and log
(define (log-messages result)
  (define status (car result))
  (define messages (cdr result))
  (for ([msg messages])
    (format-log "~a" (format "~a: ~a" status msg)))
  (if (equal? (car result) 'ok)
      (format-log "~a" "SUCCESS")
      (format-log "~a" "FAILED")))

;; Crash with error
(define (kill-me)
  (sleep 2)
  (kill-thread logging-thread)
  (exit 1))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

#|
General notes
-------------

- build a pipeline of functions with threading macro ~> f g h
- each function should take a logger and config (f logger config)
- each function should return a state object: ('ok) ('error "error string")
- if a function receives a state with car 'error it should immediately return it
- As the state list passes through functions it should build up success/error messages
- messages should be logged at the end
|#

;; Sit on the result thread and print results until 'TERMINATE
(define (print-results result)
  (displayln (format "PRINT: ~a" result))
  result)

(define (nautilus-go)
  ;; Crash if sqlite3 is not available
  (when (not (sqlite3-available?))
    (println "SQLite3 is not available on this system!")
    (kill-me))

  (create-logging-thread (hash-ref (current-config) 'logfile-path))

  ;; Get the repo and setup branch
  (define state (get-repo format-log '(ok)))

  ;; if repo setup failed, crach out
  (when (equal? (car state) 'error)
    (begin
      (log-messages state)
      (kill-me)))

  ;; Setup SQLite connections and make sure tables exist
  (define conn (create-connection (hash-ref (current-config)
                                            'sqlite-path)))
  (create-tables conn)

  ; Append SQLite connection and logger to config hash for use
  ; in other modules
  (define newconfig (~> (current-config)
                        (hash-set 'sqlite-conn conn)
                        (hash-set 'logger format-log)))

  ;;(define file-chan (create-channel))
  (define db-chan (make-channel))
  (define result-chan (make-channel))

  #|
  (walk-dirs file-chan) ; pushes to file-chan
  (fetch-papers file-chan db-chan) ;pull from file-chan pushes to db-chan
  (write-metadata db-chan result-chan); pull from db-chan pushed to result-chan
  |#

  #|

  1. walk the READMEs
  2. convert the READMEs into metadata.json files and *save*
  3. put the metadata hash into the semanticscholar-chan
  4. scholar workers make HEAD reqs for each url and save status in JSON
  5. scholar workers make API hits to semantic-scholar and save updates in JSON
  6. scholar workers put updated hash into db-chan
  7. db worker pulls hashes from db-chan and INSERTs into SQLite
  8. db worker puts hash into metadata-chan
  9. metadata-worker pulls hashes from metadata-chan and writes metadata.md

  |#

  (parameterize ([current-config newconfig])
    (define db-thread
      (create-channel-interceptor db-chan result-chan))

    (define result-thread
      (create-channel-sink result-chan print-results))

    (walk-dirs db-chan)

    ;(thread-wait result-thread)

    #|
    (define result
    (~> state
    walk-dirs
    ;process-readmes
    ;process-papers
    ;push-repo
    ))

    (log-messages result)
    |#
    (sleep 2) ; allow time to flush the log)
    (kill-thread logging-thread)))
