#lang info
(define collection "nautilus")
(define deps '("base"
               "gregor"
               "levenshtein"
               "rackunit-lib"
               "threading"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/nautilus.scrbl" ())))
(define pkg-desc "Gathers paper metadata for Papers We Love")
(define version "0.1")
(define pkg-authors '(DarrenN))
