;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/slash-meta-game.lisp is part of Tootsville
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

(defun endpoints-page-header ()
  (list "<!DOCTYPE html>
<html><head><title>Services: "
        (string-capitalize (machine-instance))
        " at "
        (long-site-name)
        "</title>
<link rel=\"stylesheet\"
 href=\"https://jumbo.tootsville.org/Assets/Styles/meta-game.services.css\"
 type=\"text/css\">
<script>
function perform_delete() {
alert(\"This software is not trustworthy enough to allow DELETE testing.\");
}
function perform_get(form) {
var uri = \"\";
for (var i = 0; i < form.childNodes.length; ++i) {
 var el = form.childNodes[i];
 if (el.tagName == \"TT\")
 { uri = uri + el.textContent; }
 if (el.tagName == \"FIELDSET\" &&
 el.childNodes[ el.childNodes.length - 1 ].tagName == \"INPUT\")
 { uri = uri + el.childNodes[ el.childNodes.length - 1 ].value; }
}
 window.open(uri);
}
</script>
</head><body>
<h1>Services</h1>

<p> Services browser for this host.  Access to these services is subject
to   the   terms  of   service   of   the   Tootsville  game;   see   <a
href=\"https://tootsville.org/\">Tootsville.org</a>. </p>

<p>  This documentation,  and much  more, can  also be  found in  the <a
href=\"http://goethe.tootsville.org/devel/docs/Tootsville/"
        (romance-ii-program-version)
        "/Tootsville.pdf\">Reference
 Manual</a>. </p>

 <h2>"
        (cluster-name)
        "</h2><h3>"
        (machine-instance)
        "."
        (short-site-name)
        "</h3><ul>"
        #(#\newline #\newline)))

(defun endpoints-page-footer ()
  "</ul></body></html>")

(defun endpoint->plist (endpoint)
  (list :method (endpoint-method endpoint)
        :template (endpoint-template endpoint)
        :content-type (endpoint-content-type endpoint)
        :fn (endpoint-function endpoint)
        :docstring (documentation (endpoint-function endpoint) 'function)
        :variables (remove-if-not #'symbolp (endpoint-template endpoint))))

(defun enumerate-routes ()
  (sort
   (sort
    (mapcar #'endpoint->plist (enumerate-endpoints))
    #'string<
    :key (rcurry #'getf :method))
   #'string<
   :key (lambda (r) (format nil "~{/~a~}" (getf r :template)))))

(defun replace-texinfo-tables (string)
  (substitute
   #\sub #\<
   (with-output-to-string (o)
     (with-input-from-string (s string)
       (loop for line = (let ((l (read-line s nil nil)))
                          (when l (string-trim " " l)))
          until (and line
                     (<= 6 (length line))
                     (string-equal "@table" line :end2 6))
          do (princ line o)
          do (terpri o)
          do (unless line
               (return-from replace-texinfo-tables string)))
       (princ "<dl>" o)
       (let ((dtp nil))
         (loop for line = (let ((l (read-line s nil nil)))
                            (when l (string-trim " " l)))
            until (and line
                       (<= 10 (length line))
                       (string-equal "@end table" line :end2 10))
            do (when line
                 (if (and (<= 5 (length line))
                          (string-equal "@item" line :end2 5))
                     (prog1
                         (if dtp
                             (princ line o)
                             (format o "</dd><dt>~a" line))
                       (setf dtp t))
                     (prog1
                         (if dtp
                             (format o "</dt><dd>~a" line)
                             (princ line o))
                       (setf dtp nil))))
            do (terpri o)
            finally
              (progn
                (if dtp
                    (princ "</dt></dl>" o)
                    (princ "</dd></dl>" o))
                (loop for line = (read-line s nil nil)
                   while line
                   do (princ line o)
                   do (terpri o)))))))))

(defun docstring->html (docstring symbol)
  (check-type symbol symbol)
  (when (fboundp symbol)
    (let ((first-line (subseq docstring 0 (position #\Newline docstring))))
      (loop for word in (function-lambda-list (fdefinition symbol))
         unless (member word lambda-list-keywords)
         do (setf first-line
                  (regex-replace-all (concatenate
                                      'string
                                      "("
                                      (string-upcase
                                       (etypecase word
                                         (atom word)
                                         (cons
                                          (etypecase (car word)
                                            (atom (car word))
                                            (cons (caar word))))))
                                      ")")
                                     first-line
                                     "@var{\\1}")))
      (setf docstring
            (concatenate 'string first-line
                         (subseq docstring (or (position #\Newline docstring)
                                               (length docstring)))))))
  (when (search "@table" docstring)
    (setf docstring (replace-texinfo-tables docstring)))
  (concatenate 'string
               "<section><p>"
               (regex-replace-pairs '(
                                      ("&" "&amp;")
                                      ("<" "&lt;")
                                      (#.(string #\sub) . "<")
                                      ("\\n\\n" . "</p><p>")
                                      ("`([A-Z0-9+/-])'" . "<b class=\"fn-ref\">\\1</b>")
                                      ("@url{(.*?)}" . "<a href=\"\\1\">\\1</a>")
                                      ("@samp{(.*?)}" . "<tt>\\1</tt>")
                                      ("@var{(.*?)}" . "<span class=\"var\">\\1</span>")
                                      ("@section{(.*?)}" . "</section><section><h3>\\1</h3>")
                                      ("@subsection{(.*?)}" . "<h4>\\1</h4>")
                                      ("@subsubsection{(.*?)}" . "<h5>\\1</h5>")
                                      ("@enumerate" . "<ol>")
                                      ("@end enumerate" . "</ol>")
                                      ("@itemize" . "<ul>")
                                      ("@end itemize" . "</ul>")
                                      ("@emph{(.*?)}" . "<em>\\1</em>")
                                      ("@item" . "<li>")
                                      )
                                    docstring)
               "</p></section>"))

(defun docstring->markdown (docstring)
  (when (search "@table" docstring)
    (setf docstring (replace-texinfo-tables docstring)))
  (concatenate 'string
               (regex-replace-pairs '(
                                      ("`([A-Z0-9+/-])'" . "`\\1`")
                                      ("@url{(.*?)}" . "\\1")
                                      ("@samp{(.*?)}" . "`\\1`")
                                      ("@var{(.*?)}" . "`\\1`")
                                      ("@section{(.*?)}" . "\\1\\n==========\\n\\n")
                                      ("@subsection{(.*?)}" ."\\1\\n==========\\n\\n")
                                      ("@subsubsection{(.*?)}" . "\\1\\n==========\\n\\n")
                                      ("@enumerate" . "")
                                      ("@end enumerate" . "")
                                      ("@itemize" . "")
                                      ("@end itemize" . "")
                                      ("@emph{(.*?)}" . "_\\1_")
                                      ("@item" . " • ")
                                      )
                                    docstring)))

(defun decorate-method-html (method)
  (format nil "<span class=\"method method-~(~a~)\">~:*~a</span>"
          method))

(defun decorate-route-template-html (template variables method)
  (if (null variables)
      (if (eql :get method)
          (format nil "<a href=\"~a\">~:*~a</a>" template)
          template)
      (if (member method '(:get :delete))
          (progn (setf template (concatenate 'string
                                             "<form  onsubmit=\"perform_"
                                             (string-downcase method)
                                             "(this)\">"
                                             template
                                             "</tt> &nbsp; <input type=\"submit\" class=\"submit-"
                                             (string-downcase method)
                                             "\" name=\"_\" value=\""
                                             (string-capitalize method)
                                             "\"></form><tt>"))
                 (dolist (variable variables template)
                   (setf template
                         (regex-replace (concatenate 'string "\\:"
                                                     (string-downcase variable))
                                        template
                                        (format nil "</tt>
<fieldset><legend><label for=\"~a\" class=\"var-label\">~:(~:*~a~)</label></legend> ~
<input type=\"text\" name=\"~:*~a\"></fieldset><tt>" variable)))))
          (dolist (variable variables template)
            (setf template
                  (regex-replace (concatenate 'string "\\:"
                                              (string-downcase variable))
                                 template
                                 (format nil "</tt><span class=\"var\">~:(~a~)</span><tt>"
                                         variable)))))))

(defun route->html (route)
  (concatenate 'string "<li>"
               (decorate-method-html (getf route :method))
               " <tt class=\"uri\">"
               (decorate-route-template-html (format nil "~{/~a~}" (getf route :template))
                                             (remove-if-not #'symbolp (getf route :template))
                                             (getf route :method))
               "</tt> (→ "
               (string-downcase (getf route :content-type))
               ") <br>"
               (docstring->html (getf route :docstring)
                                (getf route :fn))
               "</li>" #(#\Newline)))

(defun template->openapi (template)
  "Convert URI TEMPLATE into an OpenAPI template string."
  (format nil "/~{~a~^/~}"
          (mapcar (lambda (element)
                    (etypecase element
                      (string element)
                      (keyword (format nil "{~:(~a~)}" element))))
                  template)))

(defun concat (&rest args)
  (format nil
          "~{~a~^~%~}" args))

(defun find-var-in-docstring (variable docstring)
  (let ((doc-stream (make-string-input-stream docstring))
        docs)
    (loop with goal = (concatenate 'string "@item " (string variable))
       for line = (read-line doc-stream nil nil)
       while line
       when (string-equal (string-trim +whitespace+ line) goal)
       do (loop for line₂ = (read-line doc-stream nil nil)
             while line₂
             until (or (and (<= 5 (length line₂))
                            (string-begins "@item" line₂))
                       (and (<= 3 (length line₂))
                            (string-begins "@end" line₂)))
             do (push line₂ docs)
             finally (setf docs (nreverse docs))))

    (unless docs
      (setf docs (format nil "The ~:(~a~) of: ~a" variable
                         (first-line docstring))))
    (docstring->markdown (reduce #'concat docs))))

(defun find-results-in-docstring (docstring)
  (let ((doc-stream (make-string-input-stream docstring)))
    (loop
       for line = (read-line doc-stream nil nil)
       while line
       when (string-begins "@subsection{Status:" line)
       collect (list*
                (multiple-value-bind (status-number status-number-end)
                    (parse-integer (string-trim +whitespace+ (subseq line 20))
                                   :junk-allowed t)
                  (list (princ-to-string status-number)
                        (string-trim +whitespace+
                                     (subseq line (+ 20 status-number-end)
                                             (position #\} line :from-end t)))))
                (loop
                   for line₂ = (read-line doc-stream nil nil)
                   while line₂
                   until (or (string-begins "@subsection" line₂)
                             (string-begins "@section" line₂))
                   collect line₂ into description
                   finally (return (docstring->markdown
                                    (reduce #'concat description))))))))

(defun route->openapi (route)
  "Convert a ROUTE description PList into an OpenAPI description. "
  (check-type route proper-list)
  (list (string-downcase (getf route :method))
        (let ((partial
               (list :|summary|
                     (docstring->markdown (getf route :docstring))
                     :|responses|
                     (plist-hash-table
                      (mapcan (lambda (results)
                                (destructuring-bind (status+summary . description)
                                    results
                                  (destructuring-bind (status summary)
                                      status+summary
                                    (list status
                                          (plist-hash-table
                                           (list :|description| description
                                                 :|summary| summary))))))
                              (find-results-in-docstring (getf route :docstring)))))))
          (if (getf route :variables)
              (list* :|parameters|
                     (route-vars->openapi route)
                     partial)
              partial))))

(defun path->openapi (route-group)
  "Given a path list ROUTE-GROUP, return an OpenAPI URI string.

The path  list ROUTE-GROUP consists  a URI template of  constant strings
and variables as symbols and a list of routes which share that template,
each of which is a PList  with a :METHOD, :TEMPLATE, :CONTENT-TYPE, :FN,
and :DOCSTRING."
  (destructuring-bind (uri &rest routes) route-group
    (check-type uri proper-list)
    (check-type routes proper-list)
    (list (template->openapi uri)
          (plist-hash-table (mapcan #'route->openapi routes)))))

(defun route-vars->openapi (route)
  (loop for var in (getf route :variables)
     collecting
       (list :|name| (symbol-munger:lisp->camel-case var)
             :|in| "query"
             :|description| (find-var-in-docstring var (getf route :docstring))
             :|required| t
             :|schema| (list :|type| "string"))))

(defun routes-prefixed (routes)
  (let ((map (group-by routes :test 'equal
                       :key (lambda (route)
                              (format nil "~{~a/~}"
                                      (butlast
                                       (split-sequence #\/
                                                       (getf route :template))))))))
    (loop for row in map
       for (prefix . routes) = row
       when (search "maintenance" prefix)
       do (setf (cdr row) nil))
    map))

(defun group-plists (plists key)
  "Group PLISTS into a containing Alist by KEY.

Each value of KEY in the proper-list  of Plists PLISTS will be an unique
key in the resulting Alist."
  (let (retval
        current-value
        current-group)
    (flet ((maybe-save-current-group ()
             (when current-group
               (push (cons current-value (nreverse current-group)) retval)
               (setf current-group nil))))
      (dolist (plist plists)
        (let ((value (getf plist key)))
          (unless (equal value current-value)
            (maybe-save-current-group))
          (setf current-value value)
          (push plist current-group)))
      (maybe-save-current-group)
      (nreverse retval))))

(defpost group-plists ()
  (equalp (group-plists '((:a 'foo :b 1) (:a 'foo :b 2) (:a 'bar :b 3)) :a)
          '(('foo (:a 'foo :b 1) (:a 'foo :b 2)) ('bar (:a 'bar :b 3)))))



(defendpoint (get "/meta-game/services" "text/html" .15)
  "Provide a listing of services available in this cluster.

 This provides a browseable catalog of  web services that are provided by
 this machine or its siblings."
  (list 200 ()
        (reduce (curry #'concatenate 'string)
                (flatten
                 (list
                  (endpoints-page-header)
                  (mapcar (lambda (prefix-group)
                            (format nil "~%<h2>~a</h2>~{~%~a~}"
                                    (car prefix-group)
                                    (with-timeout (.001)
                                        ; The  timeout   is  because  of
                                        ; something  that spiralled  out
                                        ; to doom in this area …
                                      (mapcar #'route->html
                                              (sort
                                               (sort
                                                (cdr prefix-group)
                                                #'string-lessp
                                                :key (rcurry #'getf :method))
                                               #'string-lessp
                                               :key (lambda (r)
                                                      (format nil "~{/~a~}" (getf r :template))))))))
                          (sort (routes-prefixed (enumerate-routes))
                                #'string-lessp
                                :key #'car))
                  (endpoints-page-footer))))))

(defendpoint (get "/meta-game/services/old" "application/json")
  "This is a sketchy  sort of listing of services in  a JSON format that
 is not  anybody's standard. It  exists as  a stop-gap measure  until the
 OpenAPI form is working nicely."
  (list 200 () (list :services (enumerate-routes))))

(defendpoint (get "/meta-game/services"
                  "application/vnd.oai.openapi;version=3.0")
  "Enumerate services for OpenAPI.

Provide an  OpenAPI JSON dump  of the  same information seen  on this
page, but in a machine-readable format.

@subsection{Status: 200 OK}

The data  returned is  in the  JSON encoded form  of OpenAPI  3.0.0; see
@url{https://openapis.org/} for details."
  (list 200 () (jonathan:to-json
                (list :|openapi| "3.0.0"
                      :|info| (list :|version| (romance-ii-program-version)
                                    :|title| (romance-ii-program-name)
                                    :|license| (list :|name| "AGPLv3"))
                      :|servers| (list (list :|url| (format nil "https://users.~a.tootsville.org/users/" *cluster*)))
                      :|paths|
                      (plist-hash-table
                       (mapcan #'path->openapi
                               (group-plists (enumerate-routes) :template)))
                      :|components| #()))))

(defendpoint (get "/meta-game/headers" "application/json")
  "This method returns to the user, the headers that reached the application server.

Note  that these  may have  been modified  by proxies  or load-balancers
in transit."
  (list 200 ()
        (list :headers-in
              (alist-plist (hunchentoot::headers-in*)))))

(defendpoint (get "/meta-game/ping" "text/plain")
  "Ping the server (test server presence and latency)

@subsection{200: OK}

This endpoint always returns the 6-character string: @samp{\"pong\"}"
  (list 200 () "\"pong\""))
