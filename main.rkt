#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(module+ test
  ;; Tests to be run with raco test
  )

;; Command line takes one arg: path to .nautilusrc
(module+ main
  (require racket/cmdline
           racket/file
           json
           "private/init.rkt"
           "private/parameters.rkt")

  (define default-configfile-path
    (build-path (current-directory) ".nautilusrc"))

  ;; If no cmdline path to config file then use default path
  (define config-path
    (let* ([args (current-command-line-arguments)]
           [arg0 (if (zero? (vector-length args))
                     #f
                     (vector-ref args 0))])
      (if (path-string? arg0)
          (path->complete-path arg0)
          default-configfile-path)))

  (define (hash-merge a b)
    (for/hash ([key (hash-keys a)])
      (if (hash-has-key? b key)
          (values key (hash-ref b key))
          (values key (hash-ref a key)))))

  ;; Read config file and parse into hash
  (define (read-config path)
    (if (and (path? path) (file-exists? path))
        (call-with-input-file path
          (λ (in)
            (define json (read-json in))
            (hash-merge (current-config) json)))
        (current-config)))

  (parameterize ([current-config (read-config config-path)])
    (nautilus-go)))
