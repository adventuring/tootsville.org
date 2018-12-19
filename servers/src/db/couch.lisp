(in-package :Tootsville)

(defun connect-mixer ()
  (setf clouchdb:*couchdb*
        (ignore-errors
          (clouchdb:set-connection :host (or (config :mixer :host))
                                   :port (or (config :mixer :port) "5984")
                                   :user (config :mixer :admin :name)
                                   :password (config :mixer :admin :password)
                                   :name "tootsville/5")))
  (v:info :mixer "MOTD from Mixer: ~a"
          (cdr (assoc :|motd| (clouchdb:get-document "motd")))))


(defun find-active-Toot-for-user (&optional (user *user*))
  nil)

(defun link-active-Toot-to-user (Toot &optional (user *user*))
  (error 'unimplemented))

