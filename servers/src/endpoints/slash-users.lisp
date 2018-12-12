(in-package :Tootsville)

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
  (with-user ()
    (list 200 nil
          (list :hello "Hello, new user"
                :fake "This is a totes fake response"
                :toots "/users/me/toots.json"))))

(defendpoint (put "/users/me" "application/json")
  "Registers a new user account.

Requires the user  to pass some external,  trusted authentication source
information, like an OAuth2 login.

@subsection{Status: 201 Created}

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
  "Enumerates all Toot characters available to you."
  (with-user ()
    (list 200 (list :last-modified (header-time (yesterday)))
          (list :toots (player-toots)))))

(defendpoint (put "/users/me/toots/:toot-name" "application/json")
  "Create a new Toot character named TOOT-NAME.

Requires player authentication.

@subsection{Status: 201 Created}
Returns the Toot's information upon success.

See GET /users/me/toots/:toot-name for the format.

@subsection{Status: 307 Redirect}
If the Toot had been previously created, returns a redirect (307)."
  (with-user ()
    (assert-my-character toot-name)
    (list 201 nil (toot-info (find-toot-by-name toot-name)))))

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
