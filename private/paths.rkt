#lang racket/base

(require racket/string
         racket/path)

(provide get-paper-dir)

(define (get-paper-dir repo-path-string pdf-path)
  (define path (path->string pdf-path))
  (define filename (path->string (file-name-from-path pdf-path)))
  (string-replace (string-replace path repo-path-string "") filename ""))
