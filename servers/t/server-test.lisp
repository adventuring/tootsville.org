(asdf:oos 'asdf:load-op :fiveam)

(defpackage :org.tootsville.test
  (:use :common-lisp :it.bese.fiveam))
(in-package :org.tootsville.test)

(defsuite users :description "Test users server API")
(in-suite users)

(test )
