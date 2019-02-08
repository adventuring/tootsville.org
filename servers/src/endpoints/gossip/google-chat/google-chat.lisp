(in-package :Tootsville)

(defendpoint (POST "/gossip/google/chat/event" "application/json")
  "Handle an event from Google (Hangouts) Chat."
  (let ((event (parse-json (hunchentoot:get-raw-post-body*))))
     (cond
       ((equal (getf event :|type|) "ADDED_TO_SPACE")
        (google-chat/added-to-space event))
       ((equal (getf event :|type|) "MESSAGE")
        (google-chat/message event))
       (t (verbose:warn :google-chat "Unhandled Google Chat event type ~a~%~s" (getf event :|type|) event)))))

(defun google-chat/added-to-space (event)
  (if (equal (extract event :|space| :|type|) "ROOM")
    (list :|text| (format nil "Thanks for inviting me to ~a!"
                              (extract event :|space| :|displayName|)))))

(defun google-chat/message (event)
   (let ((said (extract event :|message| :|text|)))
      (list :|text| (format nil "You said, “~a”." said)))

