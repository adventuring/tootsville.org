(cl:in-package :cl-user)
(require :drakma)
(require :alexandria)
(require :trivial-backtrace)
(defpackage rollbar
  (:use #:cl #:drakma #:alexandria)
  (:import-from :jonathan #:to-json)
  (:export
   #:configure
   #:with-configuration
   #:debugger-hook
   #:classify-error-level
   #:with-rollbar-for-debugger
   #:notify
   #:debug! #:info! #:warning! #:error! #:critical!))
(in-package :rollbar)



(defparameter *access-token* nil
  "The Rollbar access-token, created through their Web UI at:

https://rollbar.com/{team}/{project}/settings/access_tokens/
eg:
https://rollbar.com/CIWTA/Tootsville/settings/access_tokens/")

(defparameter *environment* "unknown"
  "The runtime environment (cluster or situational group) to report as.

Typically “development” or “production,” but more interesting labels are
allowed. Groups will be automatically created by Rollbar when you report to
them; no need to pre-configure anything.")

(defparameter *code-version* nil
  "The version of your source code.

Can be anything, but a Git Hash is valid, as well as a
software version.")

(defparameter *framework* (lisp-implementation-type)
  #.(concatenate 'string
                 "Any software framework which you wish to identify as; by
default, reports the name of your Lisp implementation (from
`LISP-IMPLEMENTATION-TYPE', ie, " (lisp-implementation-type) ")") )

(defparameter *server* (machine-instance)
  "The server (machine) name to report as; defaults to `MACHINE-INSTANCE'
(which is typically the hostname)")

(defun configure (&key (access-token nil access-token-present-p)
                       (environment nil environment-present-p)
                       (code-version nil code-version-present-p)
                       (framework nil framework-present-p)
                       (server nil server-present-p))
  "Sets Rollbar configuration persistently (dynamically).

Typically only invoked once at startup."
  (when access-token-present-p
    (check-type access-token string)
    (setf *access-token* access-token))
  (when environment-present-p
    (check-type environment string)
    (setf *environment* environment))
  (when code-version-present-p
    (check-type code-version (or string-designator function))
    (setf *code-version* code-version))
  (when framework-present-p
    (check-type framework string)
    (setf *framework* framework))
  (when server-present-p
    (check-type server string)
    (setf *server* server)))

(defvar *valid-notifier-levels* '("critical" "error" "warning" "info" "debug")
  "The levels which Rollbar accepts")

(defmacro with-configuration ((&rest keys
                                     &key access-token environment
                                     code-version framework server) &body body)
  "Executes BODY with Rollbar variables bound to the values given (if any).

Unmentioned keys are left unaltered."
  (declare (ignore access-token environment code-version framework server))
  `(let ((*access-token* *access-token*)
         (*environment* *environment*)
         (*code-version* *code-version*)
         (*framework* *framework*)
         (*server* *server*))
     (configure ,@keys)
     ,@body))

(defun output-for-level (level)
  "Returns a stream for logging messages of level LEVEL.

For “info” or “debug,” returns *TRACE-OUTPUT*; otherwise
*ERROR-OUTPUT*."
  (check-type level string-designator)
  (let ((level* (string-downcase level)))
    (cond
      ((or (equalp level* "info")
           (equalp level* "debug"))
       *trace-output*)
      (t *error-output*))))

(defun level-is-valid-p (level)
  "Determines whether LEVEL is a valid level indicator for Rollbar."
  (member (string-downcase level)
          *valid-notifier-levels*
          :test #'string-equal))

(defun constituent-char-p (char)
  (plusp (logand sb-impl::+char-attr-constituent+
                 (elt  (sb-impl::character-attribute-array *readtable*)
                       (char-code char)))))

(defun symbol-name-can-be-unquoted-p (symbol)
  "Decide whether a symbol name can be printed without quoting"
  (and (not (string= (let ((*print-case* :upcase))
                       (princ-to-string symbol))
                     (let ((*print-case* :downcase))
                       (princ-to-string symbol))))
       (every #'constituent-char-p (princ-to-string symbol))))

(defun package-name-can-be-unquoted-p (package-name)
  "Decide whether a package name symbol can be printed without quoting"
  (and (every #'constituent-char-p package-name)
       (equal package-name (string-upcase package-name))))

(defun symbol-is-exported-p (symbol)
  "Discover whether SYMBOL is exported from its package"
  (multiple-value-bind (_ externality)
      (find-symbol (symbol-name symbol)
                   (symbol-package symbol))
    (declare (ignore _))
    (eql :external externality)))

(defun escaped (string escape-char escaped-chars)
  "Escape characters within the string, usually by \\"
  (let ((count-escaping (count-if (lambda (ch) (member ch escaped-chars))
                                  string)))
    (if (zerop count-escaping)
        string
        (let ((output (make-string (+ (length string) count-escaping))))
          (loop with offset = 0
             for read from 0 below (length string)
             for ch = (char string read)
             when (member ch escaped-chars)
             do
               (progn (setf (char output offset) escape-char)
                      (incf offset))
             do
               (progn (setf (char output offset) ch)
                      (incf offset)))
          output))))

(defun pretty-symbol-name (symbol)
  "Format the symbol-name of SYMBOL nicely for the Rollbar report"
  (let ((first (if (symbol-name-can-be-unquoted-p symbol)
                   (string-capitalize (symbol-name symbol))
                   (concatenate 'string "|"
                                (escaped (symbol-name symbol) #\\ '(#\\ #\|))
                                "|"))))
    (cond
      ((and (< 3 (length first))
            (string-equal first "-P" :start1 (- (length first) 2)))
       (setf (char first (1- (length first))) #\p))
      ((and (< 3 (length first))
            (not (find #\- first))
            (char= #\p (char first (1- (length first))))
            (and (not (find (char first (- (length first) 2)) "aoeui"))
                 (not (and (find (char first (- (length first) 3)) "aoeui")
                           (find (char first (- (length first) 2)) "nm")))))
       (setf (char first (1- (length first))) #\P))
      (t))
    first))

(defun format-symbol-name-carefully (symbol)
  "Carefully format the symbol-name of SYMBOL"
  (check-type symbol symbol)
  (format nil "~:[|~a|~;~:(~a~)~]:~:[:~;~]~a"
          (package-name-can-be-unquoted-p
           (package-name (symbol-package symbol)))
          (package-name (symbol-package symbol))
          (symbol-is-exported-p symbol)
          (pretty-symbol-name symbol)))

(define-condition http-error (error)
  ((status :initarg :status :reader http-error-status)
   (status-text :initarg :status-text :reader http-error-status-text)
   (wanted-uri :initarg :wanted-uri :reader http-error-wanted-uri)
   (got-uri :initarg :got-uri :reader http-error-got-uri)
   (headers :initarg :headers :reader http-error-headers)))

(defun http-successful-request (uri &rest keys &key  &allow-other-keys)
  (multiple-value-bind (body status headers reply-uri body-stream body-stream-closeable-p status-line)
      (apply #'http-request uri keys)
    (declare (ignore body body-stream body-stream-closeable-p))
    (unless (< status 400)
      (error 'http-error :status status :status-text status-line
             :wanted-uri uri :got-uri reply-uri :headers headers))))

(defun rollbar-notify-deployment (&key user revision environment)
  (http-successful-request "https://api.rollbar.com/api/1/deploy/" :method :post :content-type "application/json"
                           :content (to-json (list :access-token *access-token*
                                                   :local-username user
                                                   :revision revision
                                                   :environment environment))))

(defun send-rollbar-notification (level message backtrace)
  "Send a notification to Rollbar."
  )

(defun quoted (string)
  "Return a quoted version of String"
  (with-output-to-string (s) (print string s)))

(defun redact-directory (directory)
  "Redact uninteresting parts of a directory pathname"
  (let ((relation (first directory))
        (files (rest directory)))
    (when (equal "home" (first files))
      (setf files (cddr files)
            relation "home"))
    (when (find "servers" files :test #'string=)
      (setf files (nthcdr (1+ (position "servers" files :test #'string=)) files)
            relation "server-source"))
    (values (string-capitalize relation) (mapcar #'quoted files))))

(defun sanitize-file-name (pathname)
  (unless (pathnamep pathname)
    (return-from sanitize-file-name
      (sanitize-file-name
       (read-from-string (concatenate 'string
                                      "#p"
                                      (quoted pathname))))))
  (let ((hostname (typecase (pathname-host pathname)
                    (sb-impl::unix-host
                     (string-capitalize (machine-instance)))
                    (sb-kernel:logical-host
                     (string-capitalize (slot-value (pathname-host pathname)
                                                    'sb-impl::name)))
                    (t
                     (princ-to-string (pathname-host pathname))))))
    (multiple-value-bind (path-relation path-parts)
        (redact-directory (pathname-directory pathname))
      (format nil "~a (~:(~a~))》~{~a〉~}~a (Type: ~a)~@[ (Version: ~:(~a~))~]"
              hostname
              path-relation
              path-parts
              (quoted (pathname-name pathname))
              (quoted (pathname-type pathname))
              (pathname-version pathname)))))

(defconstant +context-forms+ 4
  "How many forms' worth of context should be reported?

 Rollbar seems to insist upon 4.")

(defun gather-source-info (filename top-level-form form-number)
  "Get source code information for a frame in a backtrace"
  (declare (ignore form-number))
  (let ((pre '()) (code nil) (post '()))
    (block gather
      (with-open-file (file filename :direction :input)
        (loop for form = (ignore-errors (read file nil :eof))
           for top-level-count from 0
           until (eql :eof form)
           do (progn
                (cond
                  ((< top-level-count
                      (- top-level-form +context-forms+))
                   nil)
                  ((< (- top-level-form +context-forms+)
                      top-level-count
                      top-level-form)
                   (push form pre))
                  ((= top-level-count top-level-form)
                   (setf code form))
                  ((< top-level-form
                      top-level-count
                      (+ top-level-form 5))
                   (push form post))
                  (t (return-from gather)))))))
    (list :code code
          :context
          (list :pre (reverse pre) :post (reverse post)))))

(defun pretty-function-name (function)
  "Pretty-print the name (and type information) of FUNCTION"
  (typecase function
    (symbol (if (symbol-function function)
                (let ((*print-right-margin* 72)
                      (*print-case* :capitalize)
                      (type (sb-introspect:function-type function)))
                  (format nil "~a  ~{(~s ~^~s~^ → ~s)~}"
                          (format-symbol-name-carefully function)
                          type))
                (format-symbol-name-carefully function)))
    (string function)
    (t (princ-to-string function))))

(defun backtrace-frame-to-plist (frame)
  "Convert FRAME into a plist of the sort Rollbar likes"
  (let (plist top-level-form form-number)
    (when-let (source-position (trivial-backtrace::frame-source-pos frame))
      ;;top-level-form form-number)
      (when (search "tlf" source-position)
        (setf top-level-form (parse-integer
                              (subseq source-position
                                      (+ 3 (search "tlf" source-position)))
                              :junk-allowed t)))
      (when (search "fn" source-position)
        (setf form-number (parse-integer
                           (subseq source-position
                                   (+ 2 (search "fn" source-position)))
                           :junk-allowed t)))
      (when-let (source-filename (trivial-backtrace::frame-source-filename
                                  frame))
        (push (sanitize-file-name source-filename) plist)
        (push :source-filename plist)
        (when (and top-level-form (probe-file source-filename))
          (setf plist
                (cons (gather-source-info source-filename
                                          top-level-form form-number)
                      plist))))
      (when-let (function (trivial-backtrace::frame-func frame))
        (push (pretty-function-name function) plist)
        (push :method plist)
        (when (and (symbolp function) (symbol-function function))
          (multiple-value-bind (_0
                                positional optional
                                rest
                                keywords
                                _5 _6 _7)
              (sb-introspect:function-lambda-list function)
            (declare (ignore _0 _5 _6 _7))
            (when positional
              (push (mapcar #'pretty-symbol-name positional) plist)
              (push :args plist))
            (when (or optional rest)
              (let (varargs)
                (when optional
                  (setf varargs
                        (mapcar (lambda (arg)
                                  (if (consp arg)
                                      (let ((*print-case* :capitalize))
                                        (format nil "~a (Default: ~s)"
                                                (pretty-symbol-name
                                                 (first arg))
                                                (rest arg)))))
                                optional)))
                (when rest
                  (setf varargs (cons varargs
                                      (format nil "(&Rest: ~a)"
                                              (pretty-symbol-name rest)))))
                (push varargs plist))
              (push :varargspec plist))
            (when keywords
              (let ((*print-case* :capitalize))
                (push (mapcar (lambda (x) (format nil "~s" x)) keywords)
                      plist))
              (push :keywordspec plist))
            ))))
    plist))

(defun find-appropriate-backtrace (condition)
  (let ((trace))
    (block tracing
      (trivial-backtrace:map-backtrace
       (lambda (frame)
         (block push-frame
           (let ((func (trivial-backtrace::frame-func frame)))
             (when (or
                    (and (stringp func)
                         (string-equal func "foreign function: call_into_lisp"))
                    (and (symbolp func)
                         (equal (symbol-package func) (find-package :swank))))
               (return-from tracing))
             (when (equal func 'find-appropriate-backtrace)
               (setf trace nil)
               (return-from push-frame)))
           (push (backtrace-frame-to-plist frame) trace)))))
    (nreverse trace)))

(defun notify (level message* &key condition)
  "Sends a notification to Rollbar of level LEVEL with message MESSAGE*.

If CONDITION is given, useful information is extracted therefrom (eg,
backtrace).

Without CONDITION,  the backtrace will be  from the current (caller) context.

If unable to reach Rollbar, a SIGNAL of type CAN-NOT-REPORT will be
raised, which you can choose to CATCH or ignore.

A log entry will also be printed to *TRACE-OUTPUT* for levels “debug” or
“info,” and to *ERROR-OUTPUT* for other levels. (See
`OUTPUT-FOR-LEVEL')"
  (check-type level string-designator)
  (assert (level-is-valid-p level) (level)
          "The classification level must be one of: ~{~a~^, ~}. (Got ~a)"
          *valid-notifier-levels* level)
  (let ((message (typecase message*
                   (string message*)
                   (symbol (format-symbol-name-carefully message*))
                   (otherwise (princ-to-string message*))))
        (backtrace (find-appropriate-backtrace condition)))
    (send-rollbar-notification (string-downcase level) message backtrace)))

(defgeneric classify-error-level (condition)
  (:documentation "Given CONDITION, return the Rollbar level for it.

Methods can specialize on condition types to return specific levels, but
the defaults should be fairly sane for most users.

Note  that `SERIOUS-CONDITION'  maps  to“error,” while  `ERROR' maps  to
“critical,” to more closely match Rollbar's system.")
  (:method ((error error)) "critical")
  (:method ((serious-condition serious-condition)) "error")
  (:method ((warning warning)) "warning")
  (:method ((condition condition)) "info"))

(defun debugger-hook (condition &optional hook)
  "Take the CONDITION reported to the debugger and relay it to Rollbar.

This is usually activated through `WITH-ROLLBAR-FOR-DEBUGGER'."
  (declare (ignore hook)) ; TODO?
  (notify (classify-error-level condition)
          (princ-to-string condition)
          :condition condition))

(defun chain-debugger-hook ()
  "Create a function that calls `DEBUGGER-HOOK'.

The present value of `*DEBUGGER-HOOK*' is closed over by that
function, and will be called after calling `DEBUGGER-HOOK'."
  (let ((closed-over-hook *debugger-hook*))
    (lambda (condition &optional hook)
      (debugger-hook condition hook)
      (when closed-over-hook
        (funcall closed-over-hook condition hook)))))

(defmacro with-rollbar-for-debugger (() &body body)
  "Run BODY with `*DEBUGGER-HOOK*' bound to call Rollbar.

Any previous value of `*DEBUGGER-HOOK*' will be called after
Rollbar."
  `(let ((*debugger-hook* (chain-debugger-hook)))
     ,@body))

(defun make-level-notifier (level)
  (eval
   `(defun ,(intern (concatenate 'string (string-upcase level) "!"))
        (message* &key condition)
      ,(concatenate 'string "Report a condition to Rollbar with level "
                    level
                    ".

Calls `NOTIFY' like (NOTIFY \"" level "\" MESSAGE …).

The ! in the name is so that ROLLBAR:ERROR! does not shadow CL:ERROR,
and so that all levels share the same orthography.")
      (funcall #'notify ,level message* :condition condition))))

(map nil #'make-level-notifier *valid-notifier-levels*)
