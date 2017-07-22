#lang racket

(provide (struct-out pdf)
         (struct-out link))

(struct pdf (sha1 filename directory normalized created modified) #:transparent)
(struct link (url directory status created modified) #:transparent)
