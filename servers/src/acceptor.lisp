(in-package :Tootsville)

(defclass Tootsville-REST-acceptor (hunchentoot:easy-acceptor)
  ((hunchentoot::taskmaster
    :initform (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))
  (:default-initargs
   :error-template-directory (config :templates :errors)
    :access-log-destination (config :log :access)
    :message-log-destination (config :log :message)))

(defmethod initialize-instance :after
    ((acceptor Tootsville-REST-acceptor) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value acceptor 'hunchentoot::taskmaster)
        (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))

(defun not-found-if-null (thing)
  "If THING is null, then abort with a 404 Not Found."
  (unless thing
    (verbose:info :not-found "{~a} 404: object not found"
                  (current-thread))
    (setf (hunchentoot:return-code*)
          hunchentoot:+http-not-found+)
    (hunchentoot:abort-request-handler))
  thing)

(defgeneric respond-to-error (condition)
  (:method ((error error))
    (hunchentoot:maybe-invoke-debugger error))
  (:method ((error unimplemented))
    (verbose:info :unimplemented "Unimplemented function called: ~s" error)))

(defun request-accept-types ()
  (when-let (accept (assoc :accept (hunchentoot:headers-in*)))
    (mapcar (curry #'string-trim +whitespace+)
            (split-sequence #\, (rest accept)))))

(defun template-match (template list)
  (if (every #'stringp template)
      (equalp template list)
      (loop for tmpl in template
         for el in list
         with result = nil
         do (etypecase tmpl
              (string (unless (string= tmpl el)
                        (return nil)))
              (symbol (push el result)))
         finally (return (nreverse result)))))

(defpost acceptor-template-matches-constants ()
  (template-match '("foo" "bar" "baz") '("foo" "bar" "baz")))
(defpost acceptor-template-unifies-variables ()
  (equalp '("42" "99")
          (template-match '("foo" :bar :baz) '("foo" "42" "99"))))

(defun strip-after-sem (s)
  (if-let ((sem (position #\; s :Test #'char=)))
    (subseq s 0 sem)
    s))

(defun accept-type-equal (a b &key (allow-wildcard-p t))
  (let ((a (strip-after-sem a))
        (b (strip-after-sem b)))
    (or (string-equal a b)
        (and allow-wildcard-p
             (or (and (string-ends "/*" a)
                      (let ((slash (position #\/ a)))
                        (string-equal a b :end1 slash :end2 slash)))
                 (and (string-ends "/*" b)
                      (let ((slash (position #\/ b)))
                        (string-equal a b :end1 slash :end2 slash)))
                 (equal a "*/*")
                 (equal b "*/*"))))))

(defpost accept-type-matches-identically ()
  (accept-type-equal "text/html" "text/html"))
(defpost accept-type-matches-with-charset=utf-8 ()
  (accept-type-equal "text/html" "text/html;charset=utf-8"))
(defpost accept-type-matches-/* ()
  (accept-type-equal "text/html" "text/*"))
(defpost accept-type-matches-/*-with-charset=utf-8 ()
  (accept-type-equal "text/html" "text/*;charset=utf-8"))
(defpost accept-type-matches-*/* ()
  (accept-type-equal "text/html" "*/*"))
(defpost accept-type-does-not-match-/*-when-not-allow-wildcards-p ()
  (not (accept-type-equal "text/html" "text/*" :allow-wildcard-p nil)))

(defun find-user-for-headers (headers)
  ;; TODO … authorization credentials 
  (when-let (auth-header (assoc "Authorization" headers))
    (when-let (credentials (validate-auth-header (cdr auth-header)))
      (find-user-for-credentials credentials))))

(defun gracefully-report-http-client-error (c)
  (if (wants-json-p)
      (encode-endpoint-reply
       (list (http-status-code c)
             '(:content-type "application/json; charset=utf-8")
             (if hunchentoot:*show-lisp-backtraces-p*
                 (jonathan.encode:to-json 
                  (list :error (http-status-code c)
                        :error-message (princ-to-string c)
                        :trace (rollbar::find-appropriate-backtrace)))
                 (jonathan.encode:to-json
                  (list :error (http-status-code c)
                        :error-message (princ-to-string c))))))
      (encode-endpoint-reply
       (list (http-status-code c)
             '(:content-type "text/html; charset=utf-8")
             (pretty-print-html-error c)))))

(defmacro with-http-conditions (() &body body)
  `(handler-case (progn ,@body)
     (http-client-error (c)
       (gracefully-report-http-client-error c))))

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor Tootsville-REST-acceptor) request)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 1)))
  (verbose:info :request "{~A} Dispatching request ~s via acceptor ~s"
                (thread-name (current-thread)) request acceptor)
  (let ((hunchentoot:*request* request)
        (*user* (find-user-for-headers (hunchentoot:headers-in request))))
    (let ((method (hunchentoot:request-method*))
          (uri-parts (split-sequence #\/ (namestring (hunchentoot:request-pathname request))
                                     :remove-empty-subseqs t))
          (ua-accept (request-accept-types)))
      (with-http-conditions ()
        (setf 
         (hunchentoot:header-out :access-control-allow-headers) "Accept;Accept-Language,Content-Language,Content-Type"
         (hunchentoot:header-out :X-Tootsville-Machine) (machine-instance)
         (hunchentoot:header-out :X-Romance) (romance-ii-program-name/version)
         
         (hunchentoot:header-out :Access-Control-Allow-Origin)
         (case (cluster)
           (:devel "*")
           (otherwise (format nil "~a, ~a"
                              (cluster-name) (cluster-net-name))))
         
         (hunchentoot:header-out :X-Lisp-Version)
         (format nil "~a/~a"
                 (lisp-implementation-type)
                 (lisp-implementation-version)))
        
        (if (eql :options method)
            (progn 
              (v:info :request "Method is OPTIONS")
              (if-let (match (find-best-endpoint (make-keyword (hunchentoot:header-in* :access-control-request-method))
                                                 uri-parts ua-accept))
                (progn
                  (setf (hunchentoot:return-code*) 204 #| no content|# )
                  (v:info :request "OPTIONS reply for ~s ~s ~s"
                          (make-keyword (hunchentoot:header-in* :access-control-request-method))
                          uri-parts ua-accept)
                  (hunchentoot:send-headers)
                  nil)
                (progn 
                  (v:info :request "No match for ~s ~s ~s"
                          (make-keyword (hunchentoot:header-in* :access-control-request-method))
                          uri-parts ua-accept)
                  (error 'not-found :the "OPTIONS URI"))))
            (if-let (match (find-best-endpoint method uri-parts ua-accept))
              (destructuring-bind (endpoint &rest bindings) match
                (verbose:info :request "Calling ~s" match)
                (apply (fdefinition (endpoint-function endpoint)) bindings))
              (progn
                (verbose:info :request "No match for ~s ~{/~a~} accepting ~s"
                              method uri-parts ua-accept)
                (error 'not-found :the (format nil "The URI you requsted")))))))))

(defmethod hunchentoot:acceptor-status-message
    ((acceptor Tootsville-REST-Acceptor) HTTP-status-code
     &rest _ &key &allow-other-keys)
  ;;(declare (ignore _))
  (verbose:info 'error "~s" _)
  (unless (wants-json-p) (call-next-method))
  (when (< HTTP-status-code 400) (call-next-method))
  
  (setf (hunchentoot:content-type*)
        "application/json;charset=utf-8")
  
  (format nil "{\"error\": ~d, \"status\":\"~a\"}"
          HTTP-status-code (gethash HTTP-status-code *http-status-message*)))
