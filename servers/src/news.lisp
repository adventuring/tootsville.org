;;;; -*- lisp -*-
;;;
;;;; ./servers/src/news.lisp is part of Tootsville
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

;;;; news.lisp — latest news feed from Totsbook
(in-package :Tootsville)

(defvar *tootsbook-cache* nil
  "This is used as a cache  for the current headlines from Tootsbook for
 a short time. See *TOOTSBOOK-REFRESH-SECONDS*  for a definition of ``a
 short time.''")
(defvar *tootsbook-fetched* 0
  "At what  universal-time was  *TOOTSBOOK-CACHE* last fetched  from the
 Tootsbook RDF feed?")

(defvar *tootsbook-refresh-seconds* (* 60 39)
  "How many seconds must pass between checking Tootsbook for new headlines?")

(defun fetch-tootsbook/http ()
  "Fetch new  headlines from  Tootsbook, and store  the RDF-XML  data in
*TOOTSBOOK-CACHE*"
  (let ((rdf (cxml:parse-octets
              (drakma:http-request
               (puri:uri "http://www.tootsbook.com/tootsbook/feed/")
               :content-type "application/rdfxml")
              (cxml-dom:make-dom-builder))))
    (when rdf
      (setf *tootsbook-fetched* (get-universal-time)
            *tootsbook-cache* rdf))))

(defun tootsbook-headlines ()
  "Return  the current  (or,  at least,  fairly  recent) headlines  from
Tootsbook's RDF feed. Uses a local cache, when available.

Returns the RDF as a raw string"
  (when (> (get-universal-time)
           (+ *tootsbook-refresh-seconds* *tootsbook-fetched*))
    (fetch-tootsbook/http))
  *tootsbook-cache*)

(defun rdf-story-to-plist (story)
  "Convert and  RDF story  (DOM object)  into a  property list  with the
title, link, content, and description."
  (loop for (tag label)
     in '(("title" :title) ("link" :link)
          ("content:encoded" :content) ("description" :description))
     collect label
     collect (get-text-of-element story tag)))

(defun tootsbook-headline-stories ()
  "Returns  the  headline  elements   (DOM  objects)  from  the  current
headlines on Tootsbook."
  (dom:get-elements-by-tag-name
   (dom:document-element
    (tootsbook-headlines)) "item"))

(defun unescape-& (string)
  "Replaces SGML-style &amp; with #\&"
  (cl-ppcre:regex-replace-all
   "&#([0-9]+);"
   (cl-ppcre:regex-replace-all "&amp;" string "&")
   (lambda (target-string start end
            match-start match-end
            reg-starts reg-ends)
     (declare (ignore start end match-start match-end))
     (string
      (code-char
       (parse-integer target-string
                      :start (first-elt reg-starts) :end (first-elt reg-ends)
                      :radix 10))))))

(defpost FIXME-NAME ()
  (string= (unescape-& "We&#8217;ve")
           "We’ve"))

(defun get-text-of-element (node element)
  "Extracts  the  text  under  the  given ELEMENT  type  under  NODE  as
a singular string."
  (apply #'concatenate 'string
         (map 'list (compose #'unescape-& #'dom:node-value)
              (dom:child-nodes
               (first-elt
                (dom:get-elements-by-tag-name node element))))))

(defun tootsbook-news-plists ()
  "Returns all  headlines in Tootsbook  currently as a list  of property
lists, each made by `RDF-STORY-TO-PLIST'."
  (map 'list #'rdf-story-to-plist
       (tootsbook-headline-stories)))

(defun pretty-date (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (sec min hour date month year dow dst tz)
      (decode-universal-time universal-time)
    (declare (ignore sec dst min tz))
    (format nil "~a, the ~:r of ~a, ~d ~a"
            (nth dow
                 '(
                   "Monday" "Tuesday" "Wednesday" "Thursday"
                   "Friday" "Saturday" "Sunday" ))
            date
            (nth month
                 '("?"
                   "January" "February" "March"
                   "April" "May" "June"
                   "July" "August" "September"
                   "October" "November" "December"))
            year
            (cond
              ((<= 0 hour 6) "in the early morning")
              ((<= 7 hour 12) "in the morning")
              ((<= 13 hour 17) "in the afternoon")
              ((<= 18 hour 22) "in the evening")
              (t "at night")))))

;; (defroute "/news" () "Render the  latest news from Tootsbook into the
;;   “news”      template."      (setf      (getf      (response-headers
;;   *response*)  :x-frame-options) "SAMEORIGIN")  (render #p"news.html"
;;   (list  :headlines   (tootsbook-news-plists)  :updated  (pretty-date
;;   *tootsbook-fetched*))))
