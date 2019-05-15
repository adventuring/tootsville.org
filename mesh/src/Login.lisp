;;;; -*- lisp -*-
;;;
;;;; ./mesh/src/Login.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2016,2017  Bruce-Robert  Pocock;  ©   2018,2019  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :login)

(defun google-logged-in-p ()
  (getf *game-state* google-user))

(defun facebook-logged-in-p ()
  (getf *game-state* facebook-user))

(defun oauth-logged-in-p ()
  (getf *game-state* oauth-user))

(defun logged-in-p ()
  (or (google-logged-in-p)
      (facebook-logged-in-p)
      (oauth-logged-in-p)))

;;; This is called with the results from from FB.getLoginStatus().
(defun clear-facebook-user ()
  (setf (getf *game-state* :facebook-user) nil))

(defun facebook-user-refused-sign-in ()
  (ga "send" "event" "Facebook Sign-In" "Refused" "Refused") ;
  ;; The person is logged into Facebook but not your app.
  (clear-facebook-user))

(defun user-is-not-signed-in-to-facebook ()
  ;; The person  is not logged into  Facebook so we're not  sure if they
  ;; are logged into this app or not.
  (ga "send" "event" "Facebook Sign-In" "Not Signed In" "Not Signed In")
  (clear-facebook-user))

(defun facebook-sign-in-changed (response)
  (log "facebook-sign-in-changed")
  (log response)
  ;; The response object  is returned with a status field  that lets the
  ;;  app know the current login status  of the person. Full docs on the
  ;;  response   object  can   be   found  in   the  documentation   for
  ;;  FB.getLoginStatus().
  (case (keywordify (oget response "status"))
    (:connected (finish-facebook-sign-in))
    (:not_authorized (facebook-user-refused-sign-in))
    (otherwise (user-is-not-signed-in-to-facebook)))
  (game-status-update))

(defun check-log-in-state ()
  "Not used?"
  (callback* (#j:FB:getLoginStatus)
             facebook-sign-in-changed))


(defun sign-in-to-facebook ()
  (callback* (#j:FB:login (make-object "scope" "public_profile,email"))
             facebook-sign-in-changed))

(setf (oget* "window" "fbAsyncInit")
      (lambda ()
        (#j:FB:init
         (make-object
          "appId" "1265378050154137"
          "cookie" t ; enable cookies to  allow the server to access the
                                        ; session
          "xfbml" true    ; parse social plugins on this page
          "version" "v2.5"   ; use Graph API version 2.5
          ))

        (callback* (#j:FB:getLoginStatus)
                   facebook-sign-in-changed)))

(defun id-element (id)
  (#j:document:getElementById id))

;; Load the SDK asynchronously
(defun load-facebook-async ()
  (let ((fjs (first (#j:document:getElementsByTagName "script"))))
    (when (id-element "facebook-jssdk")
      (return-from load-facebook-async)))
  (let ((js (#j:document:createElement "script")))
    (setf (oget js "id") "facebook-jssdk")
    (setf (oget js "src") "//connect.facebook.net/en_US/sdk.js")
    ((oget (oget fjs "parentNode") "insertBefore") js fjs)))
(load-facebook-async)

(defun set-html (id html)
  (setf (oget (id-element id) "innerHTML") html))

(defun set-set (id src)
  (setf (oget (id-element id) "src") src))

(defun finish-facebook-sign-in ()
  (#j:FB:api "/me"
             (make-object "fields" "id,name_format,picture,email")
             (lambda (response)
               (cond
                 ((not (oget response "error"))
                  (let ((name (oget response "name_format")))
                    (log "Successful login for: " name)
                    (log response)
                    (set-html "facebook-name" name))
                  (set-src "facebook-photo" (oget* response "picture" "data" "url")) ;
                  (ga "send" "event" "Facebook Sign-In" "Success" "Success"))
                 (t ; error
                  (log response)))
               (setf (getf game-state :facebook-user) response)
               (game-status-update))))

(defun element-style (id)
  (oget (id-element id) "style"))

(defun show-overlay-layer ()
  (setf (oget (element-style "overlay") "display") "block"))

(defun block/none (flag)
  (if flag "block" "none"))

(defun element-display-block-p (id)
  (equal "block" (oget (element-style id) "display")))

(defun set-element-display-block (id blockp)
  (setf (oget (element-style id) "display") (block/none flag)))

(defun hide-login-overlay ()
  (set-element-display-block "login-overlay" nil))

(defun show-login-overlay ()
  (ga "send" "screenview" (make-object "screenName" "game-welcome"))
  (set-element-display-block "login-overlay" t)
  (set-element-display-block "game-welcome" nil))

(defun sign-in-layers-toggle (service signed-in-p)
  (set-element-display-block (concatenate 'string service "-signed-in")
                             (block/none signed-in-p))
  (set-element-display-block (concatenate 'string service "-not-signed-in")
                             (block/none (not signed-in-p))))

(defun show-active-overlay (overlay-active)
  (log (concatenate 'string "active overlay: " overlay-active))
  (when (not (element-style-display-block-p overlay-active))
    (ga "send" "screenview" (make-object "screenName" overlay-active)))
  (set-element-display-block overlay-active t))

(defun game-status-update ()
  (log "game-status-update")
  (cond
    ((or (not (logged-in-p))
         (getf game-state :overlay-active))
     (show-overlay-layer)
     (when (not (logged-in-p))
       (log "user not signed in"))
     (when (not (element-display-block-p "login-overlay"))
       (show-login-overlay)))
    (t (hide-login-overlay)))

  (sign-in-layers-toggle "google" (getf *game-state* :google-user))
  (sign-in-layers-toggle "facebook" (getf *game-state* :facebook-user))

  (unless (getf *game-state* :player-info)
    (log "waiting for game server reply")
    (return-from game-status-update))
  (let ((overlay-active (getf *game-state* :overlay-active)))
    (cond (overlay-active
           (show-active-overlay overlay-active)           )
          (t (log "game play mode active")))))

(defun google-sign-in-failed ()
  (#j:alert "Google sign-in failed. Please try again.")
  (ga "send" "event" "Google Sign-In" "Failed" "Failed")
  (game-status-update))

(defun google-sign-in-button ()
  (log "Google API loaded. Need a sign-in button?")
  (game-status-update)
  (let ((auth (#j:gapi:auth2:getAuthInstance)))
    (cond (((oget auth "isSignedIn"))
           (log "Guess not! Let's fix the game state.")
           (got-google-sign-in (oget auth "currentUser" "get")))
          (t (log "Draw the sign-in button.")
             (return-from google-sign-in-button
               (#j:gapi:signin2:render "my-signin2"
                                       (make-object
                                        "scope" "profile email"
                                        ;; "width": 240, "height": 50,
                                        "longtitle" true
                                        "theme" "dark"
                                        "onsuccess" got-google-sign-in
                                        "onfailure" google-sign-in-failed)))))))

(defun make-new-player ()
  (let ((nickname "Bubba"))
    (ga "send" "event" "Game Sign-In" "Make New Player" nickname)
    (list :nickname nickname
          :hit-points 10
          :max-hit-points 10
          :inventory '((:name "Blue Jeans"
                        :id #x1000
                        :equip "pants"
                        :equipped-p t
                        :durability 1
                        :repair 1
                        :max-repair 1 )))))

(defun got-google-sign-in (google-user)
  (setf (getf *game-state* :google-user) google-user)
  (let ((profile ((oget google-user "getBasicProfile"))))
    (log (concatenate 'string "Google signed in with "
                      (funcall (oget profile "getEmail"))))
    (ga "send" "event" "Google Sign-In" "Success" "Success")
    (set-html "welcome-name" (funcall (oget profile "getName")))
    (set-src "welcome-photo" (funcall (oget profile "getImageUrl")))
    (setf (getf *game-state* :player-info) (make-new-player))
    (game-status-update)))

(defun sign-in-to-mesh (response)
  (log "Sign in to game mesh now; received directory")
  (when (response) (log response))
  (ga "send" "event" "Game Sign-In" "Join Mesh" "Join Mesh")
  ;; TODO: use returned figures from join-gossip call
  (let ((rando (floor (random 10000))))
    (set (getf *game-state* :player-info)
         (make-object "id" rando "nickname" (concatenate 'string "Testing " rando)))
    (game-status-update)))

(defun google-api-token ()
  (oget (funcall (oget (getf *game-state* :google-user)
                       "getAuthResponse"))
        "id_token"))

(defun sign-in-to-game ()
  (log "Sign in to game mesh now; get directory")
  (let ((xhr (make-new 'XMLHttpRequest)))
    (ga "send" "event" "Game Sign-In" "Start" "Start")
    (xhr-set-up-post xhr "/gossip" (lambda (response)
                                     (sign-In-To-Mesh response)))
    ((oget xhr "send") (concatenate 'string "google-api-token="
                                    (google-api-token)))))

(defun quit-from-game ()
  (let ((xhr (make-new 'XMLHttpRequest)))
    (xhr-set-up-post xhr "/quit"
                     (lambda ()
                       (setf (getf *game-state* :player-info) nil)))
    (funcall (oget xhr "send"))))



(defun quit-from-google ()
  (ga "send" "event" "Google Sign-In" "Sign Out" "Sign Out")
  (let ((auth2 (#j:gapi:auth2:getAuthInstance)))
    (-then-> (oget auth2 "signOut")
             (setf (getf *game-state* :google-user) nil)
             (game-status-update))))

(defun quit-from-oauth ())
(defun quit-from-facebook ()
  (#j:FB:logout))

(defun quit ()
  (when (not (or (getf *game-state* :player-info)
                 (logged-in-p)))
    (game-status-update))
  (when (getf *game-state* :player-info) (quit-from-game))
  (when (getf *game-state* :google-user) (quit-from-google))
  (when (getf *game-state* :facebook-user) (quit-from-facebook))
  (when (getf *game-state* :oauth-user) (quit-from-oauth)))
