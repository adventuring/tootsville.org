;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/slash-users.lisp is part of Tootsville
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

(in-package :Tootsville)

(declaim (optimize (speed 3)))

(defendpoint (get "/users/me" "application/json")
  "Provides information about your user account.

Requires player authentication.

@subsection{Status: 200 OK}

You will receive some information about your user account.

The top-level keys of the JSON object are:

@table
@item toots
The names of the Toot characters that you own.
@item logins

The authentication services which you can use. Each one has its own data
elements below it.

@end table

@subsubsection{Google Auth}

Under  logins/google, we  put  your  real name,  Gmail  address, URI  of
portrait (if you have  uploaded one), and a string token  that we use to
represent you to Google. TODO document this better

@subsubsection{Child Auth}

Child accounts will have some tokens here that help us … TODO

@subsection{Status: 401 Authorization Required}

@subsection{Status: 403 Authorization Failed}

"
  ;; with-user TODO
  (list 200 nil
        (list :hello "Hello, new user"
              :fake "This is a totes fake response"
              :toots "/users/me/toots.json")))

(defendpoint (put "/users/me" "application/json")
  "Makes changes to an user account.

Requires the user  to pass some external,  trusted authentication source
information, like an OAuth2 login.

@subsection{Status: 201 Created}

XXX is there a better status for updates?

@subsection{Status: 401 Authorization Required}

@subsection{Status: 403 Authorization Failed}

@subsection{Status: 405 Not Allowed}

"
  (with-user ()
    (list 200 nil
          (list :hello "Hello, new user"
                :fake "This is a totes fake response"
                :toots "/users/me/toots.json"))))

(defendpoint (patch "/users/me" "application/json")
  "Alters information about your user account.

Requires player authentication.

Requires a body with fields to be changed, and their new values. TODO.

@subsection{Status: 200 OK}

@subsection{Status: 401 Authorization Required}

@subsection{Status: 403 Authorization Failed}

@subsection{Status: 405 Not Allowed}

"
  (with-user ()
    (error 'unimplemented)))


(defendpoint (get "/users/me/toots" "application/json")
  "Enumerate all Toot characters available to you."
  (with-user ()
    (dolist (Toot (player-Toots))
      (v:info '(:Toots) "Player ~a has Toot ~a" (person-display-name *user*) 
              (Toot-name Toot)))
    (list 200
          (list :Last-Modified (header-time 
                                (universal-to-timestamp 
                                 (loop for Toot in (player-Toots)
                                    maximizing (timestamp-to-universal
                                                (or (Toot-last-active Toot) (now)))))))
          (list :|toots| (mapcar #'Toot-info (player-Toots))))))

(defendpoint (post "/users/me/toots/:toot-name" "application/json")
  "Create a new Toot character named TOOT-NAME.

Requires player authentication.

@subsection{Status: 201 Created}
Returns the Toot's information upon success.

See GET /users/me/toots/:toot-name for the format.

@subsection{Status: 200 OK}

Toot already existed, no changes will be made.

@subsection{Status: 307 Redirect}
If the Toot had been previously created, returns a redirect (307)."
  (with-user ()
    (if-let (pre-x (ignore-errors (find-record 'Toot :name Toot-name)))
      (progn (assert-my-character pre-x)
             (list 200 nil (Toot-info pre-x)))
      (let* ((pattern (hunchentoot:parameter "pattern"))
             (pattern-color (hunchentoot:parameter "pattern-color"))
             (base-color (hunchentoot:parameter "base-color"))
             (pad-color (hunchentoot:parameter "pad-color")))
        ;; TODO: assert that player doesn't  have too many Toots
        (check-type pattern Toot-pattern-name)
        (check-type pattern-color Toot-pattern-color-name)
        (check-type base-color Toot-base-color-name)
        (check-type pad-color Toot-pad-color-name)
        (check-pattern-on-base-color pattern-color base-color)
        (let ((new-Toot
               (make-record
                'Toot
                :name Toot-name
                :avatar 1 ; UltraToot
                :pattern pattern
                :base-color base-color
                :pattern-color pattern-color
                :pad-color pad-color
                :player (person-uuid *user*)
                :last-active (now)
                :note (or (hunchentoot:parameter "note") "")
                :avatar-scale-x 1.0
                :avatar-scale-y 1.0
                :avatar-scale-z 1.0)))
          (list 201 nil (Toot-info new-Toot)))))))

(defendpoint (get "/users/me/toots/:toot-name" "application/json")
  "Gives detailed information about your Toot character TOOT-NAME.

This Toot  must be owned by  you (the logged-in user).  You will receive
details about your  own Toot, like inventory, that are  not available to
other players.

Requires player authentication.

@subsection{Status: 200 OK}

@subsection{Status: 401 Authorization Required}
No user credentials were passed.

@subsection{Status: 403 Authorization Failed}
The user credentials presented were not recognized.

@subsection{Status: 404 Not Found}

"
  (with-user ()
    (assert-my-character toot-name)
    (error 'unimplemented)))

(defendpoint (delete "/users/me/toots/:toot-name" "application/json")
  "Permanently destroys the Toot character TOOT-NAME.

This Toot  must be owned by  you (the logged-in user).

Any inventory held by  the Toot, or property owned by  the Toot, will be
released to the public domain. Players should transfer items or property
prior to deleting a Toot.

For  a time  after  a Toot's  deletion, their  name  remains locked  (to
prevent immediate impersonation).

Requires player authentication.

@subsection{Status:  202 Toot  deletion in  progress}

The  Toot  will  be  deleted,  but   it  may  not  have  completed  yet.
A subsequent, identical request can confirm.

@subsection{Status: 204 Toot deleted}

The Toot has  been deleted. Repeated calls will return  the same status,
for the duration of the name lock on the Toot.

@subsection{Status: 401 Authorization Required}

No user credentials were passed.

@subsection{Status: 403 Authorization Failed}

The user credentials presented were not recognized.

@subsection{Status: 404 Not Found}

The Toot named does not exist.

@subsection{Status: 405 Not Allowed}

The Toot named is  one that you have permission to use,  but are not the
main owner of. This is usually a child account.

"
  (with-user ()
    (assert-my-character toot-name)
    (error 'unimplemented)))

(defendpoint (post "/users/me/play-with/:toot-name" "application/json")
  "Begin playing with the Toot named TOOT-NAME.

@table
@item Toot-Name
The name of the Toot character to play with.
@end table

@subsection{Status: 200 OK}

You are now in control of this Toot. The Toot's info will be returned.

@subsection{Status: 401 Authorization Required}

No user credentials were passed.

@subsection{Status: 403 Authorization Failed}

The user credentials presented were not recognized.

@subsection{Status: 404 Not Found}

The Toot named does not exist.

@subsection{Status: 405 Not Allowed}

The Toot named is  one that you have permission to use,  but are not the
main owner of. This is usually a child account.

"
  (with-user ()
    (assert-my-character toot-name)
    (error 'unimplemented)))
