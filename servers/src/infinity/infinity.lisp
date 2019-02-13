(in-package :Tootsville)

(defvar *infinity-ops* nil)

(defun call-infinity-from-rest (method)
  (let* ((json$ (hunchentoot:post-parameters*))
         (json (jonathan.decode:parse json$)))
    (with-user ()
      (list 200 () (funcall method json *user* (user-plan *user*))))))

(defmacro definfinity (name (lambda-list user-var world-var) &body body)
  "Define an Infinity-mode “c” command NAME.

And now, let's talk about the Infinity Mode protocol.

@subsection{ History of Infinity Mode }

In the Beginning,  Tootsville used a commercial  program called SmartFox
Server as  its chat server. There  were many problems with  this, and it
didn't last long.

However, the client program (Persephone) was written to use the SmartFox
client libraries,  which were very good.  So, we kept them,  and used an
AGPL chat server  program created by Bruce-Robert  Pocock, named Braque,
to replace  the server side.  Braque was renamed Appius  Claudius Cæcus,
because SmartFox Server was from Italy,  and Appius built the Via Appia,
a highway leaving Italy.

Appius  gained friends,  all  of which  were given  the  names of  other
Romans,  and  so  the  entire  software  suite  was  nicknamed  Romance,
like Romans.

In order to  negotiate a connection between Appius  and SmartFox client,
we had to provide a version identifier,  so we set the version number to
“infinity.”

With the  adoption of the gossipnet  for Romance Ⅱ, we  had to increment
the  version from  infinity, which  brings us  to ℵ₀  (read: Alef-Null),
which is a particular kind of infinity  that is not as big as some other
kinds  of  infinity, as  silly  as  that mathematical  construction  may
sound (yes, that's real maths).

@subsection{ Wire protocols }

There are two main wire protocols; RESTful POSTs and gossipnet.

@subsubsection{ RESTful POSTs }

XXX WRITEME

@subsection{ Datagram constructions }

There are three datagram kinds used in Infinity Mode.

@enumerate

@item

The special @samp{logOK} packet type  is used only for acknowledging and
promulgating login events through the grid. This actually dates back all
the way to the SmartFox server's protocols, so it's an odd duck.

@item

Commands    that    instigate    an    action    are    identified    by
a @samp{c} attribute.

@item

Commands that provide information about the world, usually as a reaction
to another event,  are called Gatekeeper messages and  are identified by
a @samp{from} attribute.

@end enumerate

XXX WRITEME

@subsection{ logOK datagrams }

XXX WRITEME

@subsection{ Command datagrams }

Command  datagrams  may be  processed  through  either  a REST  POST  or
the Gossipnet. These represent an action or enquiry that a client is making.

XXX WRITEME

@subsection{ Gatekeeper datagrams }

Gatekeeper datagrams are found either as the response to a REST POST, or
distributed along the Gossipnet. These  represent the state of the world
at a certain point in time.

XXX WRITEME

"
  (let ((legacy-name (symbol-munger:lisp->camel-case (string name)))
        (docstring (when (stringp (first body)) (first body)))
        (body (if (stringp (first body)) (rest body) body))
        (infinity-name (intern (concatenate 'string "INFINITY-" (string name))))
        (λ-list (if (eql '&rest (first lambda-list))
                    lambda-list
                    (cons '&key lambda-list))))
    (push (cons (string name) infinity-name) *infinity-ops*)
    (push (cons legacy-name infinity-name) *infinity-ops*)
    `(progn
       (defun ,infinity-name (d u r)
         ,docstring
         (declare (ignorable u r))
         (destructuring-bind (,@λ-list) d
           ,@body))
       (defendpoint (POST ,(concatenate 'string "/world/infinity/" (string-downcase name)))
         ,docstring
         (call-infinity-from-rest  ',infinity-name )))))
