(in-package :tootsville)

(defun post/read-version-page (port)
  "Power-On-Self-Test:  Checks  that  the  server  can  respond  to  the
version-page query locally."
  (let ((retries 9))
    (tagbody retry-post
       (handler-case
           (return-from post/read-version-page
             (drakma:http-request
              (format nil "http://localhost:~d/version.txt" port)))
         (usocket:connection-refused-error (c)
           (cond ((minusp (decf retries))
                  (error "Failed POST: Can't connect to local server ~
(after retries)~%~a" c))
                 (t (format *error-output*
                            "~&~a~%Hmm, maybe we need to wait ~
a moment and try that again.~%" c)
                    (force-output *error-output*)
                    (sleep 1)
                    (go retry-post))))))))

(defun power-on-self-test (&key (exitp t))
  "Perform some sanity checking as a part of testing.

This testing should  be much more complete  than it really is  — it will
need to be expanded a great deal to increase confidence in these tests."
  (fresh-line)
  (princ "Power-on self-test:")
  (fresh-line)
  (let ((port (+ (random 10) 27700)))
    (handler-case (start :port port)
      (simple-error (c) (if (find-restart :restart-server)
                            (invoke-restart :restart-server)
                            (signal c))))
    (sleep 1/2) ; start time
 ;;; something that appears on the version page, but no error pages.
    (let ((reply (prog1 (post/read-version-page port)
                   (stop))))
      (unless (search "Bruce-Robert Pocock" reply)
        (warn "Failed POST~%got~%~a" reply)
        (if exitp
            (cl-user::exit :code 27 :abort t :timeout 5)
            (return-from power-on-self-test nil))
        nil)))
  (fresh-line)
  (princ "Passed POST")
  (fresh-line)
  (stop)
  t)

