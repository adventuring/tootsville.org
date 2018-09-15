(in-package :cl-user)
(defpackage Tootsville-test-asd
  (:use :cl :asdf))
(in-package :Tootsville-test-asd)

(defsystem Tootsville-test
  :author "Bruce-Robert Pocock <BRFennPocock@star-hope.org>"
  :license ""
  :depends-on (:Tootsville
               :prove)
  :components ((:module "t"
                        :components
                        ((:file "Tootsville"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
