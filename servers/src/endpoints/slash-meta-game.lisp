(in-package :Tootsville)

(require 'sb-introspect)

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

(defun enumerate-routes ()
  (sort
   (sort
    (mapcar (lambda (path)
              (destructuring-bind (method template length accept function) path
                (declare (ignore length))
                (list :method method :template template :content-type accept
                      :fn function
                      :docstring (documentation function 'function))))
            *paths*)
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
  (when (fboundp symbol)
    (let ((first-line (subseq docstring 0 (position #\Newline docstring))))
      (loop for word in (sb-introspect:function-lambda-list (fdefinition symbol))
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
                                      ("@item" . "<li>")
                                      )
                                    docstring)
               "</p></section>"))

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
               "</tt> <br>"
               (docstring->html (getf route :docstring)
                                (getf route :fn))
               "</li>" #(#\Newline)))

(defun template->openapi (template)
  (regex-replace-all "\\:([a-zA-Z0-9-]*)"
                     (format nil "~{/~a~}" template)
                     (lambda (whole _ __ match-start match-end ___ ____)
                       (declare (ignore _ __ ___ ____))
                       (concatenate
                        'string
                        "{"
                        (symbol-munger:lisp->camel-case (subseq whole
                                                                (1+ match-start)
                                                                match-end))
                        "}"))))

(defun route->openapi (route)
  (check-type route proper-list)
  (list (string-downcase (getf route :method))
        (let ((partial (list :|summary|
                             (getf route :docstring)
                             :|responses|
                             (list "200"
                                   (list :|description|
                                         "success (results are not documented in OpenAPI correctly. TODO)")
                                   "default"
                                   (list :|description|
                                         "failure (results are not documented in OpenAPI correctly. TODO)")))))
          (if (getf route :variables)
              (list* :|parameters|
                     (route-vars->openapi route)
                     partial)
              partial))))

(defun path->openapi (route-group)
  (destructuring-bind (uri &rest routes) route-group
    (check-type uri string)
    (check-type routes proper-list)
    (list (template->openapi uri)
          (mapcan #'route->openapi routes))))

(defun route-vars->openapi (route)
  (loop for var in (getf route :variables)
     collecting
       (list :|name| (symbol-munger:lisp->camel-case var)
             :|in| "query"
             :|description| (string-capitalize var)
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

(defun group-plists (plists key &key (test 'eql))
  (let ((hash (make-hash-table :test test)))
    (dolist (plist plists)
      (if (gethash (getf plist key) hash nil)
          (appendf (gethash (getf plist key) hash nil) plist)
          (setf (gethash (getf plist key) hash nil) (list plist))))
    (hash-table-alist hash)))



(defendpoint (get "/meta-game/services" "text/html")
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
                                    (mapcar #'route->html
                                            (sort 
                                             (sort
                                              (cdr prefix-group)
                                              #'string-lessp
                                              :key (rcurry #'getf :method))
                                             #'string-lessp
                                             :key (lambda (r)
                                                    (format nil "~{/~a~}" (getf r :template)))))))
                          (sort (routes-prefixed (enumerate-routes))
                                #'string-lessp
                                :key #'car))
                  (endpoints-page-footer))))))

(defendpoint (get "/meta-game/services" "application/json")
  "This is a sketchy  sort of listing of services in  a JSON format that
is not  anybody's standard. It  exists as  a stop-gap measure  until the
OpenAPI form is working nicely."
  (list 200 () (list :services (enumerate-routes))))

(defendpoint (get "/meta-game/services/users"
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
                      (mapcan #'path->openapi
                              (group-plists (enumerate-routes) :template))
                      :|components| #()))))

(defendpoint (get "/meta-game/headers" "application/json")
  "This method returns to the user, the headers that reached the application server.

Note  that these  may have  been modified  by proxies  or load-balancers
in transit."
  (list 200 ()
        (list :headers-in
              (alist-plist (hunchentoot::headers-in*)))))

(defendpoint (get "/meta-game/ping" "text/plain")
  "This endpoint always returns the 4-character string: @samp{pong}"
  (list 200 () "pong"))
