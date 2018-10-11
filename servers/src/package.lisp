(cl:in-package :cl-user)
(in-package :cl-user)
(restas:define-module Tootsville
  (:documentation  "Let's make some noise!")
  (:use :alexandria :cl :local-time :oliphaunt :bordeaux-threads)
  (:shadowing-import-from :cl-fad #:copy-file #:copy-stream) ; conflicts with Alexandria. 
  (:import-from :envy
                #:config-env-var
                #:defconfig)
  (:import-from :datafly
                #:*connection*
                #:connect-cached
                #:encode-json)
  (:import-from :trivial-backtrace
                #:print-backtrace)
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
