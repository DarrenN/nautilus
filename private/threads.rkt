#lang racket/base

(require racket/function)

(provide create-channel-interceptor
         create-channel-sink)

;; Call proc on messages coming out of in and push result into out
(define (create-channel-interceptor in out [proc identity])
  (define th
    (thread
     (λ ()
       (let loop ()
         (define result (proc (channel-get in)))
         (channel-put out result)
         (if (not (equal? result 'TERMINATE))
             (loop)
             (kill-thread th))))))
  th)

;; Call proc on messages coming out of in
(define (create-channel-sink in [proc identity])
  (define th
    (thread
     (λ ()
       (let loop ()
         (define result (proc (channel-get in)))
         (if (not (equal? result 'TERMINATE))
             (loop)
             (kill-thread th))))))
  th)

