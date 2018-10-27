(cl:in-package :cl-user)

(asdf:defsystem dreamhost
  :version "0.1"
  :author "Bruce-Robert Pocock <brpocock@ciwta.org>"
  :license "AGPL v3+"
  :description "Access the Dreamhost API"

  :depends-on (
               :drakma)
:components ((:file "dreamhost")))
