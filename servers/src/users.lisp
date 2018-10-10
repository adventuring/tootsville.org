(in-package :Tootsville)



;;; Toot character data.

(defun find-toot-by-name (toot-name)
  (check-type toot-name toot-name)
  (remove-if-not (lambda (toot)
                   (string-equal (getf toot :name) toot-name))
                 (player-toots)))

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



(defun find-player-or-die ()
  "Ensure that a recognized player is connected."
  (find-user-from-session :if-not-exists :error))

(defvar *403.json-bytes*
  (flexi-streams:string-to-octets "{\"error\":\"player-not-found\",
\"note\":\"You are not signed in to the web services\",
\"login\":\"https://play.Tootsville.org/login/\"}"))

(defmacro with-player (() &body body)
  "Ensure that a recognized player is connected
using `FIND-PLAYER-OR-DIE' and bind *USER*"
  `(multiple-value-bind  (foundp *user*)
       (find-player-or-die)
     (cond (foundp
            ,@body)
           (t (return-from endpoint
                (list 403 nil *403.json-bytes*))))))



(defun assert-my-character (toot-name &optional (user *user*))
  "Signal a security error if TOOT-NAME is not owned by USER"
  (check-type toot-name toot-name)
  (unless (find toot-name
                (mapcar (rcurry #'getf :name) (player-toots user))
                :test #'string-equal)
    (error 'not-your-toot-error :name toot-name)))

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

(defun find-toot-by-name (toot-name)
  (check-type toot-name toot-name)
  (remove-if-not (lambda (toot)
                   (string-equal (getf toot :name) toot-name))
                 (player-toots)))
