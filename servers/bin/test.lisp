;;;; -*- lisp -*-
;;;
;;;; ./servers/bin/test.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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

(dolist (asd-path (directory #p"~/Private/ciwta/src/lib/alexandria/*.asd"))
  (format *trace-output* "~&;;* ASDF file: ~a~%" asd-path)
  (with-open-file (asd asd-path :direction :input)
    (handler-case
        (asdf:load-asd asd-path)
      (error (error)
        (warn "Error reading system definition file ~a: ~a"
              asd-path error)))
    (let ((systems
           (loop
              for index from 1

              for form =
                (handler-case
                    (read asd nil nil)
                  (error (error)
                    (warn "Error reading ~:r form in ~a: ~a"
                          index asd-path error)))

              while form
              when (and (listp form)
                        (string-equal (first form) "DEFSYSTEM"))
              collect (second form))))
      (cond ((null systems)
             (warn "No DEFSYSTEM forms found in ~a" asd-path))
            (t
             (format *trace-output*
                     "~&;;* Testing ~r systems from ~a (~{~a~^, ~})~%"
                     (length systems)
                     asd-path
                     systems)
             (dolist (system systems)
               (format *trace-output* "~&;;* System: ~a~%" system)
               (handler-case
                   (ql:quickload system)
                 (error (error)
                   (warn "Error loading system ~a: ~a"
                         system error)))
               (handler-case
                   (asdf:test-system (asdf:find-system system))
                 (error (error)
                   (warn "Can't test system ~a: ~a"
                         system error)))))))))
