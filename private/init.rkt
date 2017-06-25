#lang racket

(require threading
         (except-in "hash.rkt" get)
         "git.rkt"
         "logger.rkt")

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

(define (nautilus-go config)
  (create-logging-thread (hash-ref config "logfile-path"))

  (define result
    (~>> '(ok)
         (get-repo format-log config)))

  (log-messages result)
  (sleep 2) ; allow time to flush the log
  (kill-thread logging-thread))
