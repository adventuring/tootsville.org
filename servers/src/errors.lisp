(in-package :tootsville)

(defun condition-name (condition)
  (string-capitalize (symbol-name (class-name (class-of condition)))))

(defun condition-slots (condition)
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of condition))))

(defun slot-values (obj)
  (loop for slot in (condition-slots obj)
        collecting
        (list :slot (symbol-name slot)
              :value (jonathan.encode:%to-json (slot-value obj slot)))))

(defparameter +backtrace-regex+ "\\n\\w*\\d+:"
  "A regular expression to split backtraces")

(defun split-backtrace (str)
  (ppcre:split +backtrace-regex+ str))

(defun parse-backtrace (bt)
  (destructuring-bind (header &rest frames) (split-backtrace bt)
    (let ((error-msg (subseq header
                             (position #\: header :from-end t)))
          (date-time (subseq header
                             (1+ (position #\: header))
                             (position #\A header))))
      (list error-msg date-time frames))))

(defmethod jonathan.encode:%to-json ((pathname pathname))
  (jonathan.encode:%to-json
   `(:is-a "pathname"
<<<<<<< HEAD
           :host ,(typecase (pathname-host pathname)
                    (sb-impl::unix-host (machine-instance))
                    (t (princ-to-string (pathname-host pathname)))) 
           :device ,(pathname-device pathname)
           :directory ,(uiop:split-string (pathname-directory pathname) 
                                          :separator "/")
           :name ,(pathname-name pathname)
           :version ,(pathname-version pathname)
           :type ,(pathname-type pathname))))
=======
     :host ,(typecase (pathname-host pathname)
              (sb-impl::unix-host (machine-instance))
              (t (princ-to-string (pathname-host pathname))))
     :device ,(pathname-device pathname)
     :directory ,(uiop:split-string (pathname-directory pathname)
                                    :separator "/")
     :name ,(pathname-name pathname)
     :version ,(pathname-version pathname)
     :type ,(pathname-type pathname))))
>>>>>>> 850e070... reformat; merge cleanups

(defmethod jonathan.encode:%to-json ((function function))
  (let ((name (nth-value 2 (function-lambda-expression #'jonathan.encode:%to-json))))
    (jonathan.encode:%to-json
     `(:is-a "function"
       :package ,(string-upcase (package-name (symbol-package name)))
       :name ,(string-upcase (symbol-name name))))))

(defmethod jonathan.encode:%to-json ((object t))
  (jonathan.encode:%to-json
   `(:is-a ,(string-capitalize (type-of object))
     :t ,(format nil "~s" object))))

(defmethod render (bt condition env)
  (let* ((backtrace (parse-backtrace bt)))
    (encode-json
     `(:error ,(princ-to-string condition)
       :condition ,(condition-name condition)
       :location ,(if (tootsville:developmentp)
                      backtrace
                      (nth 0 backtrace))
       :slots ,(slot-values condition)
       :timestamp ,(nth 1 backtrace)
       :env ,env
       ))))

(defun present-error-to-client (condition env)
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

(defun middleware (app) 
  (lambda (env)
    (tagbody do-over
       (restart-bind 
           ((retry-request
<<<<<<< HEAD
             (lambda ()
               (go do-over))))
         (handler-bind 
=======
              (lambda ()
                (go do-over))))
         (handler-bind
>>>>>>> 850e070... reformat; merge cleanups
             ((error (lambda (c) (present-error-to-client c env))))
           (funcall app))))))
