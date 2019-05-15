;;;; -*- lisp -*-
;;;
;;;; ./servers/src/infrastructure.lisp is part of Tootsville
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

(defun choose-next-cloud (server-kind)
  "This uses the  values in *SCALING$$$* (read aloud  as: “scaling price
ratios”) to determine  which cloud provider will be chosen  for the next
host's deployment.

The intention is that the numbers  given in *SCALING$$$* are meant to be
the  relative  desireability  of  deploying  to  each  hosting  service.
Particularly,   the  hourly   cost   of  operating   a   host  on   that
cloud, inverted. (A host that costs 35¢ / hour would instead be 1/0.35.

The assignment algorithm goes as follows:

@itemize
@item

For the given server-type, there should be at least 2 clouds represented
if  there are  at least  2  hosts running.  This means,  if all  running
servers are on the same cloud, the next one should not be on that same cloud.

@item

If automatic  monitoring of a cloud  is able to determine  that there is
some error  or warning condition  exists, then  it will be  removed from
consideration for launching new instances.

@item

When allocating hosts, once any clouds are removed as required, then the
remaining clouds  are compared against  the relative numbers  of servers
that each currently  has running (of this type). The  next cloud will be
chosen in such a way that it will most closely match the relative scalar
of *SCALING$$$*.

@end itemize

"
  (declare (ignore server-kind))
  nil)
