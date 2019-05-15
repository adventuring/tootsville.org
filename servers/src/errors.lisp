(in-package :Tootsville)


;;; Convenience functions for examining conditions/errors

(defun condition-name (condition)
  "Returns the capitalized name of the class of CONDITION."
  (string-capitalize (symbol-name (class-name (class-of condition)))))

(defun condition-slots (object)
  "Enumerates the name of every slot on OBJECT"
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun slot-values (object)
  "For any OBJECT, this returns a list; each element is a PList with a slot
 name and value, encoded in JSON."
  (loop for slot in (condition-slots object)
     collecting
       (list :slot (symbol-name slot)
             :value (jonathan.encode:%to-json (slot-value object slot)))))


;;; Backtrace handling

(defparameter +backtrace-regex+ "\\n\\w*\\d+:"
  "A regular expression to split backtraces")

(defun split-backtrace (str)
  "Split a string backtrace into parts"
  (ppcre:split +backtrace-regex+ str))

(defun parse-backtrace (bt)
  "Break lines of a backtrace into error messag, date/time, and call frames
 (stack)"
  (destructuring-bind (header &rest frames) (split-backtrace bt)
    (let ((error-msg (subseq header
                             (position #\: header :from-end t)))
          (date-time (subseq header
                             (1+ (position #\: header))
                             (position #\A header))))
      (list error-msg date-time frames))))


;;; JSON encodings — XXX move these somewhere more reasonable

(defmethod jonathan.encode:%to-json ((pathname pathname))
  "Encode PATHNAME as a JSON object"
  (jonathan.encode:%to-json
   `(:is-a "pathname"
           :host ,(typecase (pathname-host pathname)
                    #+sbcl (sb-impl::unix-host (machine-instance))
                    (t (princ-to-string (pathname-host pathname))))
           :device ,(pathname-device pathname)
           :directory ,(uiop:split-string (pathname-directory pathname)
                                          :separator "/")
           :name ,(pathname-name pathname)
           :version ,(pathname-version pathname)
           :type ,(pathname-type pathname))))

(defmethod jonathan.encode:%to-json ((function function))
  "Encode FUNCTION as a JSON object."
  ;; TODO try to extract its source tree and pass it along, as well.
  (let ((name (nth-value 2 (function-lambda-expression #'jonathan.encode:%to-json))))
    (jonathan.encode:%to-json
     `(:is-a "function"
             :package ,(string-upcase (package-name (symbol-package name)))
             :name ,(string-upcase (symbol-name name))))))

(defmethod jonathan.encode:%to-json ((object t))
  "Return a JSON object that represents the state of OBJECT"
  (jonathan.encode:%to-json
   `(:is-a ,(string-capitalize (type-of object))
           :t ,(format nil "~s" object))))


;;; Rendering a backtrace … who defines this generic function? XXX
;;; XXX how is this not CLIM:PRESENT or so?

(defmethod render (bt condition env)
  "☠ deprecated"
  (let* ((backtrace (parse-backtrace bt)))
    (encode-json
     `(:error ,(princ-to-string condition)
              :condition ,(condition-name condition)
              :location ,(if hunchentoot:*show-lisp-backtraces-p*
                             backtrace
                             (nth 0 backtrace))
              :slots ,(slot-values condition)
              :timestamp ,(nth 1 backtrace)
              :env ,env
              ))))

;;; XXX make sure we don't use this

(defun present-error-to-client (condition env)
  "☠ deprecated, do not use"
  (let ((backtrace (with-output-to-string (stream)
                     (write-string
                      (print-backtrace condition
                                       :output nil)
                      stream))))
    (list 500 '(:content-type "application/json;charset=utf-8")
          (render-json (list
                        :backtrace backtrace
                        :condition condition
                        :env env)))))

;;; this might actually be TRTTD with RESTAS — or some variation XXX

(defun middleware (app)
  "☠ deprecated"
  (lambda (env)
    (tagbody do-over
       (restart-bind
           ((retry-request
             (lambda ()
               (go do-over))))
         (handler-bind
             ((error (lambda (c) (present-error-to-client c env))))
           (funcall app))))))
