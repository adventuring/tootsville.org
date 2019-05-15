(asdf:oos 'asdf:load-op :fiveam)

(defpackage :org.Tootsville.test
  (:use :common-lisp :it.bese.fiveam))
(in-package :org.Tootsville.test)

(defsuite users :description "Test users server API")
(in-suite users)

(test )
