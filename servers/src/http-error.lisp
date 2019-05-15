;;;; -*- lisp -*-
;;;
;;;; ./servers/src/http-error.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2016,2017  Bruce-Robert  Pocock;  ©   2018,2019  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :Tootsville)


(defun flatten-plist-tree (node &optional (prefix ""))
  (loop for (key value) on node by #'cddr
     for hier-key = (concatenate 'string prefix (string key))
     appending
       (etypecase value
         (cons (flatten-plist-tree value (concatenate 'string hier-key "/")))
         (atom (list (make-keyword hier-key) value)))))

(defpost unit-test-flatten-plist-tree ()
  (equalp (flatten-plist-tree '(:a (:b 2 :c (:d 42)) :e 5))
          (list :A/B 2 :A/C/D 42 :E 5)))

(defmethod acceptor-status-message ((acceptor Tootsville-REST-acceptor)
                                    http-status-code &rest properties &key &allow-other-keys)
  "Add interesting values that can be used in templates for variable-substitution"
  (let ((more-properties (append properties
                                 (flatten-plist-tree (version-info-list))
                                 (list :machine-instance (machine-instance)
                                       ))))
    (call-next-method acceptor http-status-code :properties more-properties)))
