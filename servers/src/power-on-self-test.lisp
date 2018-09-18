(in-package :tootsville)

(defvar *post-tests-queue*)

(defmacro defpost (name (&key) &body body)
  "Define  a power-on-self-test  from  somewhere else  in the  codebase.
  These are run as confidence tests  after a build, or during Production
  boot-up sequence."
  (let ((fn-name (intern (concatenate 'string "⊕POST-" (string name)))))
    `(progn
       (defun ,fn-name () ,@body)
       (pushnew ',fn-name *post-tests-queue*))))


(defun post/read-version-page (port)
  "Power-On-Self-Test:  Checks  that  the  server  can  respond  to  the
version-page query locally."
  (let ((retries 9))
    (tagbody retry-post
       (handler-case
           (return-from post/read-version-page
             (drakma:http-request
              (format nil "http://localhost:~d/version/about.txt" port)))
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
                (let ((reply (prog1 (post/read-version-page port)
                               (stop))))
                  (unless (search "Bruce-Robert Pocock" reply)
                    (warn "Failed POST~%got~%~a" reply)
                    (if exitp
                        (cl-user::exit :code 27 :abort t :timeout 5)
                        (return-from power-on-self-test nil))
                    nil)))
      (stop :port port))))


(defun power-on-self-test (&key (exitp t))
  "Perform some sanity checking as a part of testing.

This testing should  be much more complete  than it really is  — it will
need to be expanded a great deal to increase confidence in these tests."
  (fresh-line)
  (princ "Power-on self-test:")
  (fresh-line)
  (let ((warnings 0) (errors 0))
    (dolist (test *post-tests-queue*)
      (handler-case
          (funcall test)
        (warning (incf warnings))
        (error (incf errors))))
    (format t "~&Power-On Self Test: ~:[no errors~; ~r error~:p~], and ~:[no warnings~;~r warning~:p~].~&"
            errors warnings)
    (cond ((or (and (productionp) (plusp errors))
               (> errors 4)
               (> warnings 5))
           (princ "POST Failed")
           nil)
          (t (princ "POST Passed")
             t))))


