#lang racket/base

(require json
         racket/string
         simple-http
         levenshtein)

(provide fetch-semanticscholar)

(define post-template
  (hasheq 'autoEnableFilters #t
          'page 1
          'pageSize 10
          'sort "relevance"
          'authors '()
          'coAuthors '()
          'venues '()
          'facets (hasheq)
          'yearFilter (json-null)
          'enableEntities #f
          'requireViewablePdf #f))

(define (l-distance t1 t2)
  (string-levenshtein (string-trim t1) (string-trim t2)))

(define semanticscholar
  (update-ssl
   (update-host json-requester "www.semanticscholar.org") #t))

(define (search title)
  (post semanticscholar "/api/1/search"
        #:data (jsexpr->string (hash-set post-template 'queryString title))))

(define (get-text key result)
  (hash-ref (hash-ref key result) 'text))

(define (parse-result result)
  ;(println result)
  (hasheq
   'title (get-text result 'title)
   'year (get-text result 'year)
   'abstract (get-text result 'paperAbstract)
   'venue (get-text result 'venue)
   'structuredAuthors (hash-ref result 'structuredAuthors)
   'authors (hash-ref result 'authors)
   'tags (hash-ref result 'keyPhrases)
   'slug (hash-ref result 'slug)))

(define (http-error title)
  (λ (e) (list 'ERROR
               (format "Couldn't fetch ~a: ~a"
                       title (exn:fail:network:http:error-code e)))))

(define (http-read-error title)
  (λ (e) (list 'ERROR
               (format "Couldn't read data for ~a: ~a"
                       title (exn:fail:network:http:error-code e)))))

; Return the search result closest to the search query using L-distance (hasheq)
; munged into a hasheq that maps to the DB schemas
(define (fetch-semanticscholar title)
  ; catch simple-http exceptions
  (with-handlers
    ([exn:fail:network:http:error? (http-error title)]
     [exn:fail:network:http:read? (http-read-error title)])

    ; make request and process
    (define resp (search title))
    (define results (hash-ref (json-response-body resp) 'results))
    (define result-dict
      (for/hash ([result results])
        (values (hash-ref (hash-ref result 'title) 'text) result)))

    (define sorted (sort
                    (map (λ (t) (list (l-distance title t) t))
                         (hash-keys result-dict)) #:key car <))

    (define closest-match
      (if (pair? sorted)
          (car (cdar sorted))
          #f))

    (if closest-match
        (parse-result (hash-ref result-dict closest-match))
        (list 'ERROR (format "No results for ~a" title)))))
