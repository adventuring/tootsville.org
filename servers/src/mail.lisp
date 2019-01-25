(in-package :Tootsville)

(define-condition mail-error (error)
  ())

(define-condition mail-error-no-user (mail-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "There is no address that would receive this mail."))))
(define-condition mail-error-permission-denied (mail-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "You are not allowed to send mail to this address."))))
(define-condition mail-error-temp-failure (mail-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "A temporary error occurred, ~
which may be resolved in future."))))

(defmacro with-mail-errors (() &body body)
  `(handler-bind
       ((mail-error-no-user
         (lambda (c)
           (declare (ignore c))
           (sb-posix:exit #|EX_NOUSER|# 67)))
        (mail-error-permission-denied
         (lambda (c)
           (declare (ignore c))
           (sb-posix:exit #|EX_NOPERM|# 77)))
        (mail-error-temp-failure
         (lambda (c)
           (declare (ignore c))
           (sb-posix:exit #|EX_TEMPFAIL|# 75)))
        (error (lambda (c)
                 (declare (ignore c))
                 (sb-posix:exit #|EX_SOFTWARE|# 70))))
     ,@body))



(defun mail%skip-until-to-line (message-stream discard-stream)
  (loop
     (let ((line (read-line message-stream)))
       (when (string-equal "To:" (subseq line 0 3))
         (return line))
       (write line :stream discard-stream))))

(defun mail%parse-to-line (line)
  (assert (string-equal "To:" (subseq line 0 3)))
  (let ((dest (string-downcase (string-trim " " (subseq line 3)))))
    (multiple-value-bind (name domain)
        (darts.lib.email-address:parse-rfc5322-mailbox dest)
      (assert (or (string-equal "members.tootsville.com" domain)
                  (string-equal "members.tootsville.org" domain)
                  (string-equal "members.tootsbook.com" domain)
                  (string-equal "members.toots.one" domain)))
      (unless (potential-Toot-name-p name)
        (error 'mail-error-no-user))
      name)))

(defun forward-mail-to-player (message-stream)
  (with-mail-errors ()
    (let* ((prelude (make-string-output-stream))
           (name (mail%skip-until-to-line message-stream prelude))
           (Toot (find-record 'Toot :name name))
           (person (and Toot (Toot-player Toot))))
      (when person
        person))))

(defun forward-mail-string-to-person (string)
  (forward-mail-to-player (make-string-input-stream string)))
