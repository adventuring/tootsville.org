;;;; -*- lisp -*-
;;;
;;;; ./servers/src/power-on-self-test.lisp is part of Tootsville
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

(defvar *post-tests-queue* nil
  "Power-on-self-tests are placed into this queue, usually by DEFPOST.")

(defmacro defpost (name (&key) &body body)
  "Define  a power-on-self-test  from  somewhere else  in the  codebase.
 These are run as confidence tests  after a build, or during Production
 boot-up sequence."
  (let ((fn-name (intern (concatenate 'string "⊕POST-" (string name)))))
    `(progn
       (defun ,fn-name () 
         (declare (optimize (speed 0)))
         (block ,name ,@body))
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
                 (t (v:error :power-on-self-test
                             "~&~a~%Hmm, maybe we need to wait ~
a moment and try that again.~%" c)
                    (force-output *error-output*)
                    (sleep 1)
                    (go retry-post))))))))

(defpost post-version-check ()
  (let ((port (+ (random 10) 27700)))
    (unwind-protect
         (progn (handler-case (start :port port :fullp nil)
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


(defun power-on-self-test (&key (exitp nil))
  "Perform some sanity checking as a part of testing.

This testing should  be much more complete  than it really is  — it will
need to be expanded a great deal to increase confidence in these tests."
  (format t "~2&Starting Power-On Self-Test … ~a" (now))
  (let ((warnings 0) (serious 0) (errors 0) (started (get-internal-real-time)))
    (dolist (test *post-tests-queue*)
      (handler-case
          (funcall test)
        (warning (c)
          (v:error :power-on-self-test "~&WARNING: ~s~%~:*~A" c)
          (uiop/image:print-condition-backtrace c :stream *error-output*)
          (incf warnings))
        (error (c)
          (v:error :power-on-self-test "~&ERROR: ~s~%~:*~A" c)
          (uiop/image:print-condition-backtrace c :stream *error-output*)
          (incf errors))
        (serious-condition (c)
          (v:error :power-on-self-test "~&SERIOUS-CONDITION: ~s~%~:*~A" c)
          (uiop/image:print-condition-backtrace c :stream *error-output*)
          (incf serious))))
    (format t "~&~a~%Power-On Self Test completed in ~a with ~
 ~[no errors~; ~:*~r error~:p~],
~[no other serious conditions~;~:*~r other serious condition~:p~], ~
 and~[ no warnings~; ~:*~r warning~:p~].~&"
            (now)
            (human-duration (/ (- (get-internal-real-time) started) internal-time-units-per-second))
            errors serious warnings)
    (cond ((or (and (eql :prod (cluster)) (plusp errors))
               (> serious 3)
               (> errors 0)
               (> warnings 9))
           (v:error :power-on-self-test "POST Failed")
           (if exitp
               (cl-user::exit :code 27 :abort t :timeout 5)
               nil))
          (t (v:info :power-on-self-test "POST Passed")
             t))))
