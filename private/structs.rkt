#lang racket/base

(provide (struct-out author)
         (struct-out author-paper)
         (struct-out link)
         (struct-out paper)
         (struct-out pdf)
         (struct-out paper-response)
         (struct-out tag)
         (struct-out tag-paper))

(struct pdf (sha1 title filename directory created modified) #:transparent)
(struct link (url title directory status created modified) #:transparent)
(struct paper (title year abstract venue directory created modified) #:transparent)
(struct author (name first_name middle_name last_name) #:transparent)
(struct tag (tag) #:transparent)
(struct author-paper (author_id paper_id) #:transparent)
(struct tag-paper (tad_id paper_id) #:transparent)
(struct paper-response (id type record result) #:transparent)
