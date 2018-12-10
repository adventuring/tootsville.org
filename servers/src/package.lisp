(cl:in-package :cl-user)
(in-package :cl-user)

#+sbcl
(require 'sb-introspect)

(defpackage Tootsville
  (:documentation  "Let's make some noise!")
  (:use :alexandria :cl :local-time :bordeaux-threads
        :oliphaunt)
  (:shadowing-import-from :cl-fad
                          #:copy-file #:copy-stream ; conflicts with Alexandria.
                          #:directory-pathname-p)
  (:import-from :trivial-backtrace #:print-backtrace)
  (:import-from :sb-introspect #:function-lambda-expression #:function-lambda-list)
  (:import-from :uiop #:run-program)
  (:export
   #:*application-root*
   #:*compiled*
   #:config
   #:connection-settings
   #:db
   #:entry
   #:journal
   #:middleware
   #:power-on-self-test
   #:print-help
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

(defpost ensure-package-imports-from-oliphaunt-are-available ()
  (macro-function 'Tootsville::define-memo-function) ()
  "Did not import symbols correctly; `DEFINE-MEMO-FUNCTION' should ~
 be a macro, but it is not.

Got: ~a"
  (with-output-to-string (*standard-output*)
    (describe 'Tootsville::define-memo-function)))
