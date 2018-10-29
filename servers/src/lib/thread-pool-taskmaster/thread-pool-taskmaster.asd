(cl:in-package :cl-user)

(asdf:defsystem thread-pool-taskmaster
  :version "0.1"
  :author "Bruce-Robert Pocock <brpocock@ciwta.org>"
  :license "AGPL v3+"
  :description "Use a thread pool for a Taskmaster"

  :depends-on (
               :alexandria
               :fare-memoization
               :hunchentoot
               :oliphaunt
               )
  :components
  ((:file "package")
   (:file "thread-pool-taskmaster" :depends-on ("package"))))

(asdf:load-system :oliphaunt)
