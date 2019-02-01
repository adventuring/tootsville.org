;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/slash-version.lisp is part of Tootsville
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

;;;; endpoints /version/*
(in-package :Tootsville)

(defendpoint (:get "/version/about" "application/json")
  "Returns all version information about this host."
  (list 200 nil (version-info-list)))

(defendpoint (:get "/version/about" "text/plain")
  "Returns all version information about this host."
  (list 200 nil (version-info-report-string '(:*))))

(defendpoint (:get "/version/about/detail/:param" "text/plain")
  "Returns (as plain text) the info specified by PARAM.

The values available can be  seen by GET /version/about.txt, but include
the following. Values are case-insensitive.

@itemize

@item
@samp{Product}
@item
 @samp{Version}
@item
 @samp{Copyright}
@item
 @samp{Environment/Configuration}
@item
 @samp{Environment/Developmentp}
@item
 @samp{Environment/Productionp}
@item
 @samp{Machine/Version}
@item
 @samp{Machine/Type}
@item
 @samp{Machine/Instance}
@item
@samp{Site/Short-Name}
@item
@samp{Site/Long-Name}
@item
@samp{Lisp/Type}
@item
@samp{Lisp/Version}
@item
@samp{Software/Type}
@item
@samp{Software/Version}
@item
@samp{Copyright-Latest}
@item
@samp{Build-Date}
@item
@samp{Compiled}
@item
@samp{Request/Name}
@item
@samp{Request/Port}
@item
@samp{Request/Protocol}
@item
@samp{Acceptor/Class}
@item
@samp{Acceptor/Name}
@item
@samp{Acceptor/Port}
@item
@samp{Acceptor/Address}
@end itemize
"
  (if param
      (list 200 nil
            (version-info-report-string
             (uiop:split-string param :separator ".")))
      (list 400 nil
            "You forgot to ask anything.")))

(defendpoint (:get "/version/about/detail/:param" "application/json")
  "Returns (as a JSON object) the info specified by PARAM.

See the endpoint GET /version/about/detail/:param.txt  for a list  of possible
values of PARAM."
  (list 200 nil
        (list param
              (version-info-report-string
               (uiop:split-string param :separator ".")))))
