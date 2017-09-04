#lang racket/base

(provide (struct-out pdf)
         (struct-out link)
         (struct-out paper)
         (struct-out author)
         (struct-out tag)
         (struct-out author-paper)
         (struct-out tag-paper))

(struct pdf (sha1 filename directory normalized created modified) #:transparent)
(struct link (url title directory status created modified) #:transparent)
(struct paper (title year abstract venue directory created modified) #:transparent)
(struct author (name first_name middle_name last_name) #:transparent)
(struct tag (tag) #:transparent)
(struct author-paper (author_id paper_id) #:transparent)
(struct tag-paper (tad_id paper_id) #:transparent)
