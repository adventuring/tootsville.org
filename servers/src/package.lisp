(cl:in-package :cl-user)
(in-package :cl-user)
(defpackage Tootsville
  (:documentation  "Let's make some noise!")
  (:use :alexandria :cl :local-time :bordeaux-threads
        :oliphaunt)
  (:shadowing-import-from :cl-fad #:copy-file #:copy-stream) ; conflicts with Alexandria.
  (:import-from :envy
                #:config-env-var
                #:defconfig)
  (:import-from :trivial-backtrace #:print-backtrace)
  (:export
   #:*application-root*
   #:*compiled*
   #:config
   #:connection-settings
   #:db
   #:developmentp
   #:entry
   #:journal
   #:middleware
   #:power-on-self-test
   #:print-help
   #:productionp
   #:rebuild-myself
   #:render-json
   #:start
   #:start-hunchentoot
   #:start-repl
   #:start-swank
   #:stop
   #:three-chars-in-a-row-p
   #:two-chars-in-a-row-p
   #:wants-json-p
   #:with-connection
   #:write-docs
   ))

(assert (macro-function 'Tootsville::define-memo-function) ()
        "Did not import symbols correctly; `DEFINE-MEMO-FUNCTION' should ~
        be a macro, but it is not.

Got: ~a"
        (with-output-to-string (*standard-output*)
          (describe 'Tootsville::define-memo-function)))
