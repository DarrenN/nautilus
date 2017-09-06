#lang racket/base

(require racket/match
         gregor)

(provide format-log
         launch-log-daemon
         get-current-log-file)

;; logging

(define chlogger (make-logger 'nautilus))
(define rc (make-log-receiver chlogger 'info))

(define logger_thread #f)
(define current-log-file "")

;; Log listener for debug purposes. Should turn this off.
#|
(void
 (thread
  (λ ()
    (let loop ()
      (match (sync rc)
        [(vector l m v v1)
         (printf "~a\n" m)])
      (loop)))))
|#
(current-logger chlogger)

(define (get-current-log-file) current-log-file)

(define (format-log fmt . msg)
  (log-info fmt (string-append (datetime->iso8601 (now))
                               " " (apply format (cons fmt msg)))))

;; Write log messages to file

(define (start-logger log-path filename)
  (let* ([r (make-log-receiver chlogger 'info)]
         [log-date (substring (datetime->iso8601 (now)) 0 10)]
         [log-file-path (build-path log-path log-date
                               (format "~a-~a.log" filename log-date))])
    (set! current-log-file log-file-path)
    (set! logger_thread
          (thread
           (lambda ()
             (let ([log-dir (build-path log-path log-date)])
               (when (not (directory-exists? log-path))
                 (make-directory log-path))
               (when (not (directory-exists? log-dir))
                 (make-directory log-dir))
               (with-output-to-file log-file-path
                 #:exists 'append
                   (lambda ()
                     (let loop ()
                       (match (sync r)
                         [(vector l m v v1)
                          (printf "~a\n" m)
                          (flush-output)])
                       (loop))))))))))

(define (restart-logger)
  (kill-thread logger_thread)
  (start-logger))

(define (launch-log-daemon log-path filename)
  (start-logger log-path filename)
  (thread
   (lambda ()
     (let loop ()
       (sync
        (alarm-evt (+ (current-inexact-milliseconds) (* 1000 60 60))))
       (when (= 0 (date-hour (seconds->date (current-seconds))))
         (restart-logger))
       (loop)))))
