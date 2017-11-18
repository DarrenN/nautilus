#lang racket/base

(require racket/function
         racket/list)

(provide create-channel-interceptor
         create-channel-sink
         create-done-counter
         create-interceptor-pool)

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

;; Create a list of channel interceptor threads
(define (create-interceptor-pool pool-size in out [proc identity])
  (for/list ([i (range pool-size)])
      (create-channel-interceptor in out proc)))

;; Return a function that checks results for 'DONE symbols and compares to total
;; sending a 'TERMINATE when reached, otherwise passes through result
(define (create-done-counter total)
  (define done-counter (box 0))
  (define (count-done-threads d)
    (let ([ov (unbox done-counter)])
      (when (equal? d 'DONE) (box-cas! done-counter ov (add1 ov)))
      (unbox done-counter)))

  (λ (result)
    (define done-count (count-done-threads result))
    (if (>= done-count total)
        'TERMINATE
        result)))
