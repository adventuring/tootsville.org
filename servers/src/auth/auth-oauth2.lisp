(in-package :Tootsville)

;;; XXX OAuth2 could probably get abstracted out into a lib, too …

(defun b64token-p (char)
  (or (char<= #\A char #\Z)
      (char<= #\a char #\z)
      (char<= #\0 char #\9)
      (member char '(#\- #\. #\_ #\~ #\+ #\/)
              :test #'char=)))

(deftype b64token ()
  '(satisfies b64token-p))

(defun vbutlast (vector)
  (subseq vector 0 (1- (length vector))))

(defun validate-auth-header (auth-header)
  (block nil
    (let ((auth-header (string-trim +whitespace+ auth-header)))
      (unless (string= "Bearer " auth-header :end2 7)
        (return nil))
      (let ((credentials (subseq auth-header 7)))
        (unless (every #'b64token-p (vbutlast credentials))
          (return nil))
        (unless (char= #\= (last-elt credentials))
          (return nil))
        (concatenate 'string "OpenID/" credentials)))))

(defun get-openid-credentials-for-bearer-token (credentials)
  (when-let (found (clouchdb:get-document credentials))
    (make-instance 'openid-credentials
                   :aud (assoc-value found :|aud|)
                   :iat (assoc-value found :|iat|)
                   :exp (assoc-value found :|exp|)
                   :azp (assoc-value found :|azp|)
                   :iss (assoc-value found :|iss|)
                   :user (assoc-value found :|user|))))

(defun get-openid-credentials-from-database (uuid)
  (let* ((cred (find-record 'db.credentials uuid))
	 (json (db.credentials-json-info cred)))
    (make-instance 'openid-credentials
		   :aud (db.credentials-auth-token cred)
		   :iat (db.credentials-id-token cred)
		   :exp (extract json "exp")
		   :azp (extract json "azp")
		   :idd (extract json "iss")
		   :user (db.credentials-uid cred))))

(defun key-for-uuid (kind uuid)
  (concatenate 'string kind "/"
               (uuid:format-as-urn nil uuid)))

(defun get-user-by-uuid (uuid)
  (find-record 'db.person "uuid" uuid))

(defun auto-vivify-user (credentials)
  (let ((person (create-record 'db.person
			       :display-name "Needs Naming"
			       :given-name "Newbie"
			       :lang "en_US")))
    (create-record 'db.credential
		   :person (db.person-uuid person)
		   :provider "FIXME"
		   ;; FIXME auto-vivify-user credentials
		   )))
		   
    
    

(defun find-user-for-credentials (credentials)
  (when-let (cred (get-openid-credentials-for-bearer-token credentials))
    (or (get-user-by-uuid (credentials-user cred))
        (auto-vivify-user cred))))
