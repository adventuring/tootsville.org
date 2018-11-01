(cl:in-package :cl-user)

(defpackage thread-pool-taskmaster
  (:use :cl :hunchentoot :bordeaux-threads)
  (:import-from :alexandria #:when-let #:if-let)
  (:import-from :fare-memoization #:define-memo-function)
  (:import-from :oliphaunt #:processor-count)
  (:export #:thread-pool-taskmaster
           #:*developmentp*
           #:with-pool-thread-restarts))

