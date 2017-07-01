#lang racket

(provide (struct-out pdf))

(struct pdf (sha1 filename directory normalized created modified) #:transparent)
