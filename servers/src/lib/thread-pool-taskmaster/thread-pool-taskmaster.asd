(cl:in-package :cl-user)

(asdf:defsystem thread-pool-taskmaster
  :version "0.1"
  :author "Bruce-Robert Pocock <brpocock@ciwta.org>"
  :license "AGPL v3+"
  :description "Use a thread pool for a Taskmaster"

  :depends-on (
               :hunchentoot
               :restas
              )
  :components
    ((:file "../../utils")
     (:file "../../machine" :depends-on ("../../utils"))
     (:file "thread-pool-taskmaster" :depends-on ("../../machine"))))
