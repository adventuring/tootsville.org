;;;; -*- lisp -*-
;;;
;;;; ./servers/src/db/couch.lisp is part of Tootsville
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

(declaim (optimize (speed 3)))

(defun connect-mixer ()
  (setf clouchdb:*couchdb*
        (let ((conf (car (db-config :mixer))))
          (v:info :mixer "Connecting to mixer at ~a" (extract conf :host))
          (run-program (list "ssh" "-f" (extract conf :host) "-L" 
                             (format nil "27784:~a:~d"
                                     (extract conf :host)
                                     (or (extract conf :port) 5984))
                             "cat"))
          (with-timeout (2)
            (clouchdb:set-connection :host "localhost"
                                     :port "27784"
                                     :user (extract conf :player :name)
                                     :password (extract conf :player :password)
                                     :protocol "http"
                                     :name "tootsville/5"
                                     :document-fetch-fn #'identity
                                     :document-update-fn #'identity))))
  (if-let (motd (ignore-errors (clouchdb:get-document "tootsville%2f5/motd")))
    (v:info :mixer "MotD from Mixer: ~a"
            (cdr (assoc :|motd| motd)))
    (v:info :mixer "Connected to Mixer; no MotD"))
  clouchdb::*couchdb*)


(defstruct gossip-initiation
  uuid
  offeror
  offer
  answeror
  answer)

(defmethod database-for ((type (eql 'gossip-initiation)))
  (list :couch :gossip-exchange))

(defmethod db-table-for ((type (eql 'gossip-initiation)))
  "gossip-exchange")

(defmethod id-column-for ((type (eql 'gossip-initiation)))
  :|uuid|)

(defmethod %to-json ((object gossip-initiation)))

(defmethod destroy-record ((init gossip-initiation))
  (clouchdb:delete-document (to-json init)))

(defmethod save-record ((init gossip-initiation))
  (unless (gossip-initiation-uuid init)
    (setf (gossip-initiation-uuid init) (uuid:make-v1-uuid)))
  (clouchdb:put-document (to-json init)
                         :id (gossip-initiation-uri init))
  (to-json init))

(defmethod load-record ((class (eql 'gossip-initiation)) alist)
  (make-gossip-initiation :uuid (assoc-value alist :|uuid|)
                          :offeror (assoc-value alist :|offeror|)
                          :offer (assoc-value alist :|offer|)
                          :answeror (assoc-value alist :|answeror|)
                          :answer (assoc-value alist :|answer|)))

(defmethod make-record ((class (eql 'gossip-initiation)) &rest plist)
  (let ((init (apply #'make-gossip-initiation plist)))
    (save-record init)
    init))

(defmethod find-record ((class (eql 'gossip-initiation))
                        &key uuid)
  (if uuid
      (clouchdb:get-document (gossip-initiation-uri uuid)
                             :if-missing :nil)
      (error "Must provide UUID")))

(defmethod find-records ((class (eql 'gossip-initiation))
                         &key offeror
                              (answeror nil answerorp)
                              (answer nil answerp))
  (cond
    ((<= 1 (+ (if offeror 1 0)
              (if answerorp 1 0)
              (if answerp 1 0)))
     (error "Can't search that way: ~
supply exactly one of OFFEROR, ANSWEROR, ANSWER"))
    (offeror
     (mapcar (lambda (r) (load-record 'gossip-initiation r))
             (clouchdb:invoke-view
              "offeror" "offeror"
              :key (uuid-to-uri (person-uuid offeror)))))
    ((and answerp
          (null answer))
     (mapcar (lambda (r) (load-record 'gossip-initiation r))
             (clouchdb:invoke-view
              "pending" "pending")))
    ((and answerorp
          (null answeror))
     (curry #'load-record 'gossip-initiation)
     (clouchdb:invoke-view
      "unanswered" "unanswered"))
    (t (clouchdb:all-docs-by-seq))))


(defun find-active-Toot-for-user (&optional (user *user*))
  (when user
    (error 'unimplemented)))

(defun link-active-Toot-to-user (Toot &optional (user *user*))
  (declare (ignore Toot user))
  (error 'unimplemented))
