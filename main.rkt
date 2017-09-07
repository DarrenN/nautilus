#lang racket/base

(require racket/cmdline
         racket/file
         json
         "private/init.rkt"
         "private/parameters.rkt"
         "private/utils.rkt")

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

(define default-configfile-path
  (build-path (current-directory) ".nautilusrc"))

;; If no cmdline path to config file then use default path
(define (config-path)
  (let* ([args (current-command-line-arguments)]
         [arg0 (if (zero? (vector-length args))
                   #f
                   (vector-ref args 0))])
    (if (path-string? arg0)
        (path->complete-path arg0)
        default-configfile-path)))

;; Read config file and parse into hash
(define (read-config path)
  (if (and (path? path) (file-exists? path))
      (call-with-input-file path
        (λ (in)
          (define json (read-json in))
          (hash-merge (current-config) json)))
      (current-config)))

;; Command line takes one arg: path to .nautilusrc
(module+ main
  (parameterize ([current-config (read-config (config-path))])
    (nautilus-go)))

(module+ test
  (require rackunit)
  (check-pred path? default-configfile-path)
  (check-pred path? (config-path))
  (check-pred path-string? (config-path))
  (check-equal? default-configfile-path (config-path))

  ;; Check that setting arg0 changes config-path
  (define p "/bad/brains/.nautilusrc")
  (parameterize ([current-command-line-arguments (vector p)])
    (check-equal? (path->complete-path p) (config-path))))
