#lang racket/base

(provide current-config)

(define current-config
  (make-parameter
   (hasheq 'sqlite-path (build-path (current-directory) "nautilus.db")
           'pwlrepo-path (build-path (current-directory) "papers")
           'pwlrepo-hostname "github.com"
           'pwlrepo-repository "papers-we-love/papers-we-love.git"
           'logfile-path (build-path (current-directory) "logs"))))
