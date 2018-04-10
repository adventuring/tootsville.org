;;; register.lisp —  handle user  registration and sign-in  queries from
;;; the web
(in-package :tootsville)

(define-constant +google-client-ids+
    '("1052481910502-4tkek5mbv4jf9o16oaclg1v3ugrruilg.apps.googleusercontent.com"
      "1052481910502-sumg845at7g627qdvhhhambm7c7do8e4.apps.googleusercontent.com")
  :test #'equalp
  :documentation  "The  set  of  Google  client ID's  for  whom  we  are
 interested in being a proxy for OpenID. These are obtained through the
 Google   Developer   console  and   are   used   to  validate   OpenID
 sign-in requests.")

(defvar *google-public-key* nil
  "The  public key  for Google's  OpenID signatures.  This is  cached to
 validate the signatures provided by their servers.")

(defun fetch-json (uri)
  "Fetch URI  as an application/json file  and parse it with  Yason into
a property list tree."
  (yason:parse
   (map 'string #'code-char
        (drakma:http-request uri :method :get :accept "application/json;charset=utf-8"))
   :object-as :plist
   :object-key-fn (compose #'make-keyword #'string-upcase)
   :json-arrays-as-vectors t))

(defun get-google-public-key ()
  "Either fetch  the Google  public-key for OpenID,  or, use  the cached
copy in *GOOGLE-PUBLIC-KEY*"
  (or *google-public-key*
      (setf *google-public-key*
            (fetch-json (getf (fetch-json "https://accounts.google.com/.well-known/openid-configuration")
                              :jwks_uri)))))

(defun check-jwk (json-web-key header body signature)
  "TODO: Split  id-token into three  parts on #\.  and pass in  with the
public         key        from         Google.        Similar         to
http://ncona.com/2015/02/consuming-a-google-id-token-from-a-server/ "
  (declare (ignorable json-web-key header body signature))
  (error 'unimplemented))

(defun user<-google-token (id-token)
  "Given a token returned from a Google OpenID sign-in, validate that it
is good, and return the local user object reference associated with it."
  (multiple-value-bind
        (response-binary status response-headers response-uri
         stream stream-closed-p status-reason)
      (drakma:http-request "https://www.googleapis.com/oauth2/v3/tokeninfo"
                           :parameters (list (cons "id_token" id-token))
                           :method :get
                           :accept "application/json;charset=utf-8")
    (declare (ignore response-headers stream stream-closed-p))
    (assert (<= 200 status 299) ()
            "While validating Google sign-in token, error ~D: ~A returned by ~A"
            status status-reason response-uri)
    (assert (string= "https://www.googleapis.com/"
                     response-uri :end2 27) ()
                     "While validating Google sign-in token, response returned by ~A, whose authority I do not recognize"
                     response-uri)
    (let* ((response (yason:parse (map 'string #'code-char response-binary)
                                  :object-as :plist
                                  :object-key-fn (compose #'make-keyword
                                                          #'string-upcase)
                                  :json-arrays-as-vectors t)))
      (assert (find (getf response :iss) '("accounts.google.com"
                                           "https://accounts.google.com")
                    :test #'string-equal) ()
                    "While  validating Google  sign-in token,  ISS field
returned was ~A, whose authority I do not recognize"
                    (getf response :iss))
      (assert (< (parse-integer (getf response :exp))
                 (local-time:timestamp-to-unix (local-time:now))) ()
                 "The Google sign-in permission has already expired.")
      (user-id<-registrar-id :google (getf response :sub)))))



(defun render-player-details (player)
  "Return a JSON object describing the player object.

This method needs to be reworked."
  (let ((player (ensure-player player)))
    (render-json `((:player . ((:id .,(player-id player))
                               (:email .,(player-email player))
                               (:given-name .,(player-given-name player))
                               (:surname .,(player-surname player))
                               (:full-name .,(player-full-name player))
                               (:date-of-birth .,(player-date-of-birth player))
                               (:age .,(player-age player))))))))

(defroute facebook-deauthorize
    ("/login/registrars/facebook/deauthorize-callback") ()
  "This callback is invoked whenever a Facebook user de-authorizes our
access to their Facebook account/profile.  It should unlink the Facebook
user information from the Toots account. If there is no remaining way to
log  in to  the account,  though, that  effectively would  disable their
access … TODO"
  "☹")

(defroute ("/login/registrars/:registrar" :method :post)
    (&key registrar id-token)
  "Accept a POST request with information about a player sign-in through an
identity provider  identified by the case-insensitive  string REGISTRAR.
An ID-TOKEN unique among all users of REGISTRAR is required.

There is no  need for ID-TOKEN to be distinctive  of a particular player
across time or valid far into  the future. The REGISTRAR will be queried
by a specific method to obtain a reference to a local user.
"
  (handler-case
      (let ((user (ecase (make-keyword (string-upcase registrar))
                    (:google (user<-google-token id-token))
                    (:facebook (error 'unimplemented)))))
        (when user
          (render-player-details user)))
    (error (c)
      (render-json `((:error . (princ-to-string ,c)))))))
