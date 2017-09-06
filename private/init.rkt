#lang racket/base

(require db
         threading
         "db-adapter.rkt"
         "git.rkt"
         "logger.rkt"
         "pdf.rkt"
         "papers.rkt"
         "parameters.rkt"
         "readmes.rkt")

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

#|
General notes
-------------

- build a pipeline of functions with threading macro ~> f g h
- each function should take a logger and config (f logger config)
- each function should return a state object: ('ok) ('error "error string")
- if a function receives a state with car 'error it should immediately return it
- As the state list passes through functions it should build up success/error
  messages
- messages should be logged at the end
|#

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

  (parameterize ([current-config newconfig])
    (define result
      (~> state
          process-pdfs
          process-readmes
          process-papers
          push-repo))

    (log-messages result)
    (sleep 2) ; allow time to flush the log)
    (kill-thread logging-thread)))
