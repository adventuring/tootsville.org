(in-package :Tootsville)

(defvar *post-tests-queue* nil
  "Power-on-self-tests are placed into this queue, usually by DEFPOST.")

(defmacro defpost (name (&key) &body body)
  "Define  a power-on-self-test  from  somewhere else  in the  codebase.
 These are run as confidence tests  after a build, or during Production
 boot-up sequence."
  (let ((fn-name (intern (concatenate 'string "⊕POST-" (string name)))))
    `(progn
       (defun ,fn-name () (block ,name ,@body))
       (pushnew ',fn-name *post-tests-queue*))))


(defun post/read-version-page (port)
  "Power-On-Self-Test:  Checks  that  the  server  can  respond  to  the
version-page query locally."
  (let ((retries 9))
    (tagbody retry-post
       (handler-case
           (return-from post/read-version-page
             (drakma:http-request
              (format nil "http://localhost:~d/version/about" port)
              :additional-headers '(("Accept" . "text/plain"))))
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

(defpost post-version-check ()
  (let ((port (+ (random 10) 27700)))
    (unwind-protect
         (progn (handler-case (start :port port)
                  (simple-error (c) (if (find-restart :restart-server)
                                        (invoke-restart :restart-server)
                                        (signal c))))
                (sleep 1/2)        ; start time
;;; something that appears on the version page, but no error pages.
                (let ((reply (prog1 (ensure-string (post/read-version-page port))
                               (stop))))
                  (unless (search "Bruce-Robert Pocock" reply)
                    (error "Failed POST~%got~%~a" reply)
                    nil)))
      (stop))))


(defun power-on-self-test (&key (exitp t))
  "Perform some sanity checking as a part of testing.

This testing should  be much more complete  than it really is  — it will
need to be expanded a great deal to increase confidence in these tests."
  (format t "~2&Starting Power-On Self-Test … ~a" (now))
  (let ((warnings 0) (serious 0) (errors 0))
    (dolist (test *post-tests-queue*)
      (handler-case
          (funcall test)
        (warning (c)
          (format *error-output* "~&WARNING: ~s~%~:*~A" c)
          (uiop/image:print-condition-backtrace c :stream *error-output*)
          (incf warnings))
        (error (c)
          (format *error-output* "~&ERROR: ~s~%~:*~A" c)
          (uiop/image:print-condition-backtrace c :stream *error-output*)
          (incf errors))
        (serious-condition (c)
          (format *error-output* "~&SERIOUS-CONDITION: ~s~%~:*~A" c)
          (uiop/image:print-condition-backtrace c :stream *error-output*)
          (incf serious))))
    (format t "~&Power-On Self Test completed ~a with ~
 ~[no errors~; ~:*~r error~:p~],
~[no other serious conditions~;~:*~r other serious condition~:p~], ~
 and~[ no warnings~; ~:*~r warning~:p~].~&"
            (now) errors serious warnings)
    (cond ((or (and (productionp) (plusp errors))
               (> serious 6)
               (> errors 4)
               (> warnings 8))
           (princ "POST Failed")
           (if exitp
               (cl-user::exit :code 27 :abort t :timeout 5)
               nil))
          (t (princ "POST Passed")
             t))))
