(in-package :Tootsville)

(defendpoint (get "/meta-game/headers" "application/json")
  (list 200 ()
        (list :headers-in
              (alist-plist (hunchentoot::headers-in*)))))

(defendpoint (get "/meta-game/ping" "text/plain")
  (list 200 () "pong"))

(defun endpoints-page-header ()
  (list "<!DOCTYPE html>
<html><head><title>Services</title>
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
               "</tt></li>" #(#\Newline)))

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

(defendpoint (get "/meta-game/services" "text/html")
  (list 200 ()
        (reduce (curry #'concatenate 'string)
                (flatten (list (endpoints-page-header)
                               (mapcar (lambda (prefix-group)
                                         (format nil "~%<h5>~a</h5>~{~%~a~}"
                                                 (car prefix-group)
                                                 (mapcar #'route->html (cdr prefix-group))))
                                       (routes-prefixed (enumerate-routes)))
                               (endpoints-page-footer))))))

(defendpoint (get "/meta-game/services" "application/json")
  (list 200 () (list :services (enumerate-routes))))
