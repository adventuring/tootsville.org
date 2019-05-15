;;;; -*- lisp -*-
;;;
;;;; ./servers/src/db/generic-db.lisp is part of Tootsville
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



(defgeneric database-for (type)
  (:documentation
   "The database containing the data mirrored by the TYPE

Returns a  pairs with the  type of database  (:MARIA or :COUCH)  and the
database or schema identification (a proper list)."))

(defgeneric db-table-for (type)
  (:documentation
   "The database table or view containing the data mirrored by the TYPE"))

(defgeneric destroy-record (object)
  (:documentation
   "Delete the record in the database representing OBJECT.

Does  not   attempt  to  destroy   OBJECT,  so  a  subsequent   call  to
`SAVE-RECORD' could potentially be used to re-create it."))

(defmethod destroy-record :after (object)
  "Invalidate any cached version of OBJECT"
  (invalidate-cache object))

(defgeneric find-record (type &rest columns+values)
  (:documentation
   "Find a record of TYPE where each of COLUMNS+VALUES are exact matches.

Expects to find 0 or 1 result. If more results are found, signals an error.

See `FIND-RECORDS' for more details."))

(defgeneric find-records (type &rest columns+values)
  (:documentation
   "Find all records of TYPE where each of COLUMNS+VALUES are exact matches.

For each of the  columns named, the value given must  be an exact match.
In the case  of SQL, this translates neatly into  a construction such as
“WHERE column₁ = value₁, AND column₂ = value₂, … AND columnｎ = valueｎ.”
With  other kinds  of database  (e.g.  LDAP, Couch,  &c) the  equivalent
constructions will be used.

This method  is not suitable  for inequalities, set comparisons,  or the
like — in fact, only value-like equality is supported.

The function returns NIL if no records are found."))

(defgeneric find-reference (object field)
  (:documentation
   "Following the FIELD on OBJECT, return the referenced object.

Note that  this returns an  object of the  appropriate type, not  its ID
code. The regular column  reference function (CLASS)-(FIELD) will return
the ID value, which may be of any type (eg, UUID, STRING, NUMBER, &c)"))

(defgeneric id-column-for (type)
  (:documentation
   "The column (if any) providing the primary key for TYPE.

May return NIL if there is no simple primary key.")
  (:method ((x t)) (declare (ignore x)) nil))

(defgeneric invalidate-cache (object)
  (:documentation
   "Identify that the cache is dirty  and should be cleared of a certain
 set of possible records.

This   is  called   by   the  :AFTER   methods   of  `SAVE-RECORD'   and
`DESTROY-RECORD'."))

(defgeneric load-record (type columns)
  (:documentation
   "Create an object of TYPE from the raw data in COLUMNS.

Used by `FIND-RECORD'  and `FIND-RECORDS', which are what  a normal user
of this API will be interested-in."))

(defgeneric make-record (type &rest columns+values)
  (:documentation
   "Create a new record of TYPE with initial values COLUMNS+VALUES.

Implies saving that record to the backing storage, as well.

This is analogous  to `MAKE-INSTANCE' or a  `DEFSTRUCT' constructor, but
for ORM objects."))

(defgeneric save-record (object)
  (:documentation
   "Write OBJECT to the database, with any changes made.

Types are  encouraged to  introduce appropriate consistency  checks into
a  :BEFORE method  on this  function.  The default  :AFTER method  calls
`INVALIDATE-CACHE',"))

(defmethod save-record :after (object)
  "Invalidate any cached version of OBJECT"
  (invalidate-cache object))
