(in-package :Tootsville)

(defparameter *google-account-keys-refresh* (* 20 60)
  "How  often (in  sec)  to  refresh the  Google  account  keys used  in
  Firebase authentication verification?")

(defun http-fetch-json (uri &rest drakma-options)
  (jonathan.decode:parse
   (map 'string #'code-char
        (apply #'drakma:http-request uri :accept "application/json" 
               drakma-options))))

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
      (when (<= 200 http-status 299)
        (setf keys (jonathan.decode:parse
                    (map 'string #'code-char json-data))
              keys-update-next 
              (timestamp+ (now)
                          (or (let ((n (subheader-field (assoc :cache-control
                                                               headers-alist)
                                                        "max-age")))
                                (parse-integer n))
                              *google-account-keys-refresh*)
                          :seconds)))) 
    ;; FIXME: Use  the value of  max-age in the Cache-Control  header of
    ;; the  response from  that endpoint  to  know when  to refresh  the
    ;; public keys.
    (setf keys (http-fetch-json ))))

(defun check-firebase-id-token (token)
  (let* (header
         payload
         (sub (getf :|sub| payload)))
    (assert (string= "RS256" (getf :|alg| header)) (token)
            "Credentials token does not have a permitted algorithm")
    (assert (member (getf :|kid| header) (plist-keys (get-google-account-keys)))
            (token)
            "Credentials token does not have a recognized signing key ID")
    (assert (> (getf :|exp| payload) (timestamp-to-unix (now))) (token)
            "Credentials token has expired")
    (assert (< (getf :|iat| payload) (timestamp-to-unix (now))) (token)
            "Credentials token will be issued in the future. You must be punished for violating causality.")
    (assert (< (getf :|auth_time| payload) (timestamp-to-unix (now))) (token)
            "Credentials token is from a future user authentication. You must be punished for violating causality.")
    (assert (string= (getf :|aud| payload) (config :firebase :project-id)) (token)
            "Credentias token was not for us (we are not its audience)")
    (assert (stringp sub) (token)
            "Credentials token subject should be a string")
    (assert (< 4 (length sub) 256) (token)
            "Credentials token subject length seems improper.")
    (cljwt-custom:verify jwt
                         (getf (getf :|kid| header) (get-google-account-keys))
                         :rs256
                         :fail-if-unsecured t
                         :fail-if-unsupported t)
    sub))
