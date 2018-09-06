(in-package :cl-user)
(defpackage tootsville-test-asd
  (:use :cl :asdf))
(in-package :tootsville-test-asd)

(defsystem tootsville-test
  :author "Bruce-Robert Pocock <BRFennPocock@star-hope.org>"
  :license ""
  :depends-on (:tootsville
               :prove)
  :components ((:module "t"
                        :components
                        ((:file "tootsville"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
