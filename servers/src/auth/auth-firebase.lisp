(in-package :Tootsville)

(defparameter *google-account-keys-refresh* (* 20 60)
  "How  often (in  sec)  to  refresh the  Google  account  keys used  in
  Firebase authentication verification?")

(defun http-fetch-json (uri &rest drakma-options)
  (jonathan.decode:parse
   (map 'string #'code-char
        (apply #'drakma:http-request uri :accept "application/json" drakma-options))))

(let ((keys nil)
      (keys-updated (timestamp- (now) 1 :year)))
  (defun get-google-account-keys ()
    (when (< (timestamp-difference (now) keys-updated) *google-account-keys-refresh*)
      (return-from get-google-account-keys keys))
    ;; FIXME: Use  the value of  max-age in the Cache-Control  header of
    ;; the  response from  that endpoint  to  know when  to refresh  the
    ;; public keys.
    (setf keys (http-fetch-json "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"))))

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
