;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/slash-toots.lisp is part of Tootsville
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

(in-package :Tootsville)

(defendpoint (get "/toots/:toot-name/avatar" "application/json" 1)
  "Get the avatar info for TOOT-NAME"
  (check-arg-type Toot-name Toot-name)
  (if-let (Toot (find-Toot-by-name Toot-name))
    (list 200
          `(:last-modified ,(header-time (or (Toot-last-active Toot) (now))))
          (Toot-info Toot))
    (list 404 nil nil)))

(defendpoint (get "/toots/:toot-name" "application/json" 1)
  "Get public info about TOOT-NAME"
  (check-arg-type toot-name toot-name)
  (with-user ()
    (let ((Toot (find-toot-by-name toot-name)))
      (list 200
            `(:last-modified ,(header-time))
            (Toot-info Toot)))))

(defendpoint (post "/toots/:toot-name/child-p" "application/json")
  "Set CHILD-P flag for TOOT-NAME.

Input JSON: { set: true } or { set: false }"
  (check-arg-type Toot-name Toot-name)
  (with-user ()
    (let ((Toot (find-Toot-by-name Toot-name)))
      (assert-my-character Toot))
    (list 200
          `(:last-modified ,(header-time (Toot-last-active Toot)))
          (if-let (toot (find-toot-by-name toot-name))
            (toot-info toot)
            `(:is-a "toot"
                    :name ,(string-capitalize toot-name)
                    :avatar "ultraToot"
                    :child-p nil
                    :sensitive-p t
                    :online-p t
                    :last-seen ,(local-time:format-timestring
                                 nil (local-time:now))
                    :exists-p "maybe?")))))
