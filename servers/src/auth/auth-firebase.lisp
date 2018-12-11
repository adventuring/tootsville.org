(in-package :Tootsville)

(defparameter *google-account-keys-refresh* (* 20 60)
  "How  often (in  sec)  to  refresh the  Google  account  keys used  in
  Firebase authentication verification?")

(defun subheader-field (header-assoc label)
  (when header-assoc
    (let* ((label* (concatenate 'string label ":"))
           (len (length label*))
           (finds 
            (mapcar #'second
                    (split-sequence
                     ":"
                     (remove-if-not 
                      (lambda (section)
                        (and (> (length section) len)
                             (string-equal label* section
                                           :end2 len)))
                      (mapcar 
                       (curry #'string-trim +whitespace+)
                       (split-sequence ";" (cdr header-assoc))))))))
      (case (length finds)
        (0 nil)
        (1 (string-trim +whitespace+ (first finds)))
        (otherwise (warn "Multiple sub-header hits in ~:(~a~) for ~(~a~)"
                         (car header-assoc) label)
                   (string-trim +whitespace+ (first finds)))))))

(let ((keys nil)
      (keys-update-next (timestamp- (now) 1 :year)))
  (defun get-google-account-keys ()
    (when (timestamp< (now) keys-update-next)
      (return-from get-google-account-keys keys))
    (multiple-value-bind 
          (json-data http-status headers-alist reply-uri 
                     reply-stream stream-close-p status-message)
        (drakma:http-request
         "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"
         :accept "application/json")
      (when (typep http-status 'http-response-success-status-number)
        (setf keys (jonathan.decode:parse
                    (map 'string #'code-char json-data))
              keys-update-next 
              (timestamp+ (now)
                          (or (let ((n (subheader-field (assoc :cache-control
                                                               headers-alist)
                                                        "max-age")))
                                (parse-integer n))
                              *google-account-keys-refresh*)
                          :seconds))))))

(defun check-firebase-id-token (token)
  (multiple-value-bind (claims header digest claims$ header$)
      (cljwt-custom:unpack token)
    (declare (ignore claims digest claims$ header$))
    (multiple-value-bind (payload token-header)
        (cljwt-custom:verify token
                             (gethash (gethash :|kid| header) (get-google-account-keys))
                             :rs256
                             :fail-if-unsecured t
                             :fail-if-unsupported t)
      (declare (ignore token-header))
      (let ((sub (gethash :|sub| payload)))
        (assert (string= "RS256" (gethash :|alg| header)) (token)
                "Credential token does not have the required algorithm")
        (assert (member (gethash :|kid| header) (plist-keys (get-google-account-keys)))
                (token)
                "Credential token does not have a recognized signing key ID")
        (assert (> (gethash :|exp| payload) (timestamp-to-unix (now))) (token)
                "Credential token has expired")
        (assert (< (gethash :|iat| payload) (timestamp-to-unix (now))) (token)
                "Credential token will be issued in the future. ~
You must be punished for violating causality.")
        (assert (< (gethash :|auth_time| payload) (timestamp-to-unix (now))) (token)
                "Credential token is from a future user authentication. ~
You must be punished for violating causality.")
        (assert (string= (gethash :|aud| payload) (config :firebase :project-id)) 
                (token)
                "Credential token was not for us (we are not its audience)")
        (assert (stringp sub) (token)
                "Credential token's subject should be a string")
        (assert (< 4 (length sub) 256) (token)
                "Credential token's subject length seems improper.")
        (list :uid sub
              :email (gethash :|email| payload)
              :email-verified-p (equalp "true" (gethash :|email_verified| payload))
              :name (gethash :|name| payload)
              :picturo (gethash :|picture| payload))))))
