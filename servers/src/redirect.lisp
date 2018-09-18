;;;; redirect.lisp — HTTP redirect
(in-package :Tootsville)

(defun redirect-to/html-body (uri)
  "Returns an octet array that gives a simple redirection link.

This is  a silly  legacy thing  for ancient  browsers that  don't follow
a  3xx   redirection  or  want   to  display  something   while  they're
redirecting. In  real life, it's  rarely encountered by a  real browser,
but sometimes caught by tools like curl or wget with certain settings."
  (check-type uri www-uri)
  (assert (not (some (curry #'char= #\") uri)) (uri)
          "Unsafe to redirect to an URI containing literal ~
#\\Quotation_Mark characters (~a)" uri)
  (concatenate 'string
               "<!DOCTYPE html><html><title>Redirect</title><a href="
               (string #\quotation_mark)
               uri
               (string #\quotation_mark)
               ">Redirected to "
               uri
               "</a></html>"))


(defun redirect-to (uri &optional (status 307))
  "Redirect to  another URI. Status code  307 for temporary, 301  or 308
for  permanent  (typically).  (:TEMPORARY and  :PERMANENT  are  accepted
for readability.)

As a side  effect, provides an extremely skeletal  HTML redirection page
via `REDIRECT-TO/HTML/BODY'."
  (check-type uri www-uri "A URL string")
  (check-type status (member 301 307 308 :temporary :permanent)
              "An HTTP status code from among 301, 307, or 308")
  (let ((status (if (numberp status) status
                    (ecase status (:temporary 307) (:permanent 308)))))
    (list
     status
     `(:location ,uri
       :x-redirected-by ,(romance-ii-program-name/version)
       :content-type "text/html")
     (redirect-to/html-body uri))))

(defmethod on-exception (code)
  "Return error with code CODE

CODE is allowed to be a string beginning with an HTTP error code.

CODE must be between 300-599, inclusive, or 501 will be used.

TODO: We SHOULD validate that CODE is a sane HTTP error code, but we don't."
  (cond
    ((consp code)
     (render-json code))
    ((wants-json-p)
     (render-json `((:error . ,code))))
    (t (let ((code-number (typecase code
                            (number code)
                            (string (parse-integer code :junk-allowed t))
                            (t 501))))
         (unless (<= 300 code-number 599)
           (setf code-number 501))
         (redirect-to (format nil "/error/~d" code-number) :temp)))))
