#lang racket/base

(require racket/function)

(provide get-directories)

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

;; Return a list of directories without .git folders
;; (-> path (path-string))
(define (get-directories dir-path)
  (parameterize ([current-directory dir-path])
    (filter
     path?
     (for/list ([path (in-directory)])
       (when (and (directory-exists? path)
                  (not (regexp-match? #rx".git" path)))
         path)))))
