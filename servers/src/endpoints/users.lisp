(in-package :Tootsville)


(defun find-player-or-die ()
  "Ensure that a recognized player is connected."
  (find-user-from-session :if-not-exists :error))

(defvar *403.json-bytes*
  (flexi-streams:string-to-octets "{\"error\":\"player-not-found\",
\"note\":\"You are not signed in to the web services\",
\"login\":\"https://play.Tootsville.org/login/\"}"))



(define-condition not-your-toot-error (error)
  ((name :initarg name :accessor which-toot-is-not-yours))
  (:report (lambda (c s)
             (format s "You do not have a Toot named “~a.”" 
                     (which-toot-is-not-yours c)))))

(defun assert-my-character (toot-name &optional (user *user*))
  "Signal a security error if TOOT-NAME is not owned by USER"
  (check-type toot-name toot-name)
  (unless (find toot-name
                (mapcar (rcurry #'getf :name) (player-toots user))
                :test #'string-equal)
    (error 'not-your-toot-error :name toot-name)))

(defendpoint (:get "/users/me" "application/json")
  (with-player ()
    (list 200 nil
          (list :hello "Hello, new user"
                :fake "This is a totes fake response"
                :toots "/users/me/toots.json"))))

(defun days-ago (days)
  (local-time:timestamp- (local-time:now) days :day))

(defun yesterday ()
  (days-ago 1))

(defun 2-days-ago ()
  (days-ago 2))

(defun 3-days-ago ()
  (days-ago 3))

(defun header-time (&optional (time (get-universal-time)))
  (local-time:format-rfc1123-timestring
   nil
   (etypecase time
     (number (local-time:universal-to-timestamp time))
     (local-time:timestamp time))))

(defun toot-info (toot)
  (append toot (list :is-a "toot")))

(defun player-toots (&optional (player *user*))
  (declare (ignore player))
  (list
   (list :name "Zap"
         :note "These are still fake Toots for testing"
         :avatar "UltraToot"
         :base-color "violet"
         :pattern "lightning"
         :pattern-color "yellow"
         :highlight-color "yellow"
         :child-p nil
         :sensitive-p nil
         :last-seen (local-time:format-timestring
                     nil (3-days-ago)))
   (list :name "Flora"
         :note "This an an example of a child's Toot
appearing on a parent's account."
         :avatar "UltraToot"
         :base-color "pink"
         :pattern "flowers"
         :pattern-color "white"
         :highlight-color "yellow"
         :child-p t
         :sensitive-p nil
         :last-seen (local-time:format-timestring
                     nil (2-days-ago)))
   (list :name "Moo"
         :note ""
         :avatar "UltraToot"
         :base-color "white"
         :pattern "moo"
         :pattern-color "black"
         :highlight-color "black"
         :child-p nil
         :sensitive-p nil
         :last-seen (local-time:format-timestring
                     nil
                     (yesterday)))))

(defendpoint (get "/users/me/toots" "application/json")
  (with-player ()
    (list 200 (list :last-modified (header-time (yesterday)))
          (list :toots (player-toots)))))

(defun find-toot-by-name (toot-name)
  (check-type toot-name toot-name)
  (remove-if-not (lambda (toot)
                   (string-equal (getf toot :name) toot-name))
                 (player-toots)))

(defendpoint (put "/users/me/toots/:toot-name" "application/json")
  (with-player ()
    (assert-my-character toot-name)
    (list 200 nil (toot-info (find-toot-by-name toot-name)))))

(defendpoint (post "/users/me/toots" "application/json")
  (with-player ()
    (error 'unimplemented)))

(defendpoint (delete "/users/me/toots/:toot-name" "application/json")
  (with-player ()
    (assert-my-character toot-name)
    (error 'unimplemented)))

(defendpoint (get "/toots/:toot-name" "application/json")
  (check-arg-type toot-name toot-name)
  (with-player ()
    (list 200
          `(:last-modified ,(header-time))
          (if-let (toot (find-toot-by-name toot-name))
            (toot-info toot)
            `(:is-a "toot"
              :name ,(string-capitalize toot-name)
              :avatar "ultraToot"
              :child-p nil
              :sensitive-p t
              :online-p t
              :last-seen ,(local-time:format-timestring
                           nil (local-time:now))
              :exists-p "maybe?")))))
