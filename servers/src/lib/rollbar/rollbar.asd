(asdf:defsystem :rollbar
  :description "CL support for reporting to Rollbar"
  :author "Bruce-Robert Pocock"
  :version "0.1"
  :maintainer "Bruce-Robert Pocock"
  :mailto "brpocock+rollbar-lisp@star-hope.org"
  :licence "AGPLv3" ; if this poses a problem, ask me for a waiver.
  :long-name "Rollbar access from Common Lisp"
  
  :depends-on (:alexandria :drakma)
  
  :encoding :utf-8
  
  :serial t
  
  :components
  ((:file "rollbar")))
