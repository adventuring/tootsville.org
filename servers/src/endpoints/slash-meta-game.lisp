(in-package :Tootsville)

(defendpoint (get "/meta-game/headers" "application/json")
  (list 200 ()
        (list :headers-in
              (alist-plist (hunchentoot::headers-in*)))))

(defendpoint (get "/meta-game/ping" "text/plain")
  (list 200 () "pong"))

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
</head><body>
<h1>Services</h1>
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
    (mapcar (lambda (pair)
              (destructuring-bind (fn . attribs) pair
                (list* :fn fn
                       :docstring (documentation fn 'function)
                       (hash-table-plist attribs))))
            (hash-table-alist
             (gethash :routes
                      (restas::find-pkgmodule-traits :Tootsville))))
    #'string<
    :key (rcurry #'getf :method))
   #'string<
   :key (rcurry #'getf :template)))

(defun docstring->html (docstring)
  docstring)

(defun decorate-method-html (method)
  (format nil "<span class=\"method method-~(~a~)\">~:*~a</span>"
          method))

(defun decorate-route-template-html (template variables method)
  (if (null variables)
      (if (eql :get method)
          (format nil "<a href=\"~a\">~:*~a</a>" template)
          template)
      (if (eql :get method)
          (progn (setf template (concatenate 'string
                                             "<form onsubmit=\"perform_get\">"
                                             template
                                             "</form>"))
                 (dolist (variable variables template)
                   (setf template
                         (regex-replace (concatenate 'string "\\:"
                                                     (string-downcase variable))
                                        template
                                        (format nil "</tt>
<fieldset><legend><label for=\"~a\" class=\"var\">~:(~:*~a~)</label></legend> ~
<input type=\"text\" name=\"~:*~a\"></fieldset><tt>" variable)))))
          (dolist (variable variables template)
            (setf template
                  (regex-replace (concatenate 'string "\\Q:"
                                              (string-downcase variable)
                                              "\\E")
                                 template
                                 (format nil "</tt><span class=\"var\">~:(~a~)</span></tt>"
                                         variable)))))))

(defun route->html (route)
  (concatenate 'string "<li>"
               (decorate-method-html (getf route :method))
               " <tt class=\"uri\">"
               (decorate-route-template-html (getf route :template)
                                             (getf route :variables)
                                             (getf route :method))
               "</tt> <br>"
               (docstring->html (getf route :docstring))
               "</li>" #(#\Newline)))

(defun template->openapi (template)
  (regex-replace-all "\\:([a-zA-Z0-9-]*)"
                     template
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
                              (format nil "~{~a~^/~}"
                                      (butlast
                                       (split-sequence #\/
                                                       (getf route :template))))))))
    (when (equal (caar map) "")
      (setf (caar map) "/"))
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
  (list 200 ()
        (reduce (curry #'concatenate 'string)
                (flatten (list (endpoints-page-header)
                               (mapcar (lambda (prefix-group)
                                         (format nil "~%<h5>~a</h5>~{~%~a~}"
                                                 (car prefix-group)
                                                 (mapcar #'route->html
                                                         (cdr prefix-group))))
                                       (routes-prefixed (enumerate-routes)))
                               (endpoints-page-footer))))))

(defendpoint (get "/meta-game/services" "application/json")
  (list 200 () (list :services (enumerate-routes))))

(defendpoint (get "/meta-game/services/users"
                  "application/vnd.oai.openapi;version=3.0")
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
