(in-package :Tootsville)



(defun two-chars-in-a-row-p (string char-bag)
  "Do any two characters in CHAR-BAG occur together in STRING?"
  (check-type string string)
  (check-type char-bag sequence)
  (loop for i from 1 below (length string)
     when (and (find (char string i) char-bag)
               (find (char string (1- i)) char-bag))
     do (return-from two-chars-in-a-row-p i))
  nil)

(defun three-chars-in-a-row-p (string &optional char-bag)
  "Do any three characters in CHAR-BAG occur together in STRING?

If CHAR-BAG is NIL, then any  character that occurs three times matching
itself returns true."
  (check-type string string)
  (check-type char-bag (or null sequence))
  (unless (<= 3 (length string))
    (return-from three-chars-in-a-row-p nil))
  (if char-bag
      (progn
        (assert (every #'characterp char-bag))
        (loop for i from 2 below (length string)
           when (and (find (char string i) char-bag)
                     (find (char string (1- i)) char-bag)
                     (find (char string (- i 2)) char-bag))
           do (return-from three-chars-in-a-row-p i)))
      (loop for i from 2 below (length string)
         when (char= (char string i)
                     (char string (- i 1))
                     (char string (- i 2)))
         do (return-from three-chars-in-a-row-p i)))
  nil)


;;; Toot names

(defun remove-repeats-for-Toot-name (string)
  (regex-replace-all "--+"
                     (regex-replace-all "(.)\\1\\1+" string "\\1")
                     "-"))

(defun check-Toot-name (name)
  "Check if NAME is allowed as a Toot name; offering restarts to correct it,
 if not.

This  is  generally  intended  for  accepting  new  Toot  names,  versus
validating REST calls, for example."
  (tagbody do-over
     (restart-bind
         ((auto-rename
           (lambda ()
             (let ((try (remove-repeats-for-Toot-name
                         (substitute-if #\- (complement #'alpha-char-p) name))))
               (when (char= #\- (first-elt try))
                 (setf try (subseq try 1)))
               (when (char= #\- (last-elt try))
                 (setf try (subseq try 0 (- (length try) 2))))
               (when (< (length try) 3)
                 (setf try (concatenate 'string try "-a")))
               (when (< 32 (length try))
                 (setf try (subseq try 0 32)))
               try))
            :report-function
            (lambda (s)
              (format s "Find a name similar to ~a"
                      name)))
          (provide-new-name
           (lambda (new-name)
             (setf name new-name)
             (go do-over))
            :report-function
            (lambda (s) (format s "Supply a new name"))))
       (check-type name toot-name))))

(define-memo-function potential-Toot-name-character-p (character)
  "Is CHARACTER allowed in a Toot name at all?

Allowed characters are alphanumerics, apostrophe, hyphen, or space, but
there are additional rules in `POTENTIAL-TOOT-NAME-P' which limit
the string as a whole."
  (and (characterp character)
       (or (alphanumericp character)
           (char= #\- character)
           (char= #\' character)
           (char= #\space character))))

(defun potential-Toot-name-p (Toot-name)
  "Could TOOT-NAME be allowed as a Toot name?

Toot names must be:

@itemize

@item
From three to 32 characters in length, inclusive.

@item
Characters must be  `POTENTIAL-TOOT-NAME-CHARACTER-P', ie, alphanumeric,
a space, apostrophe, or hyphen.

@item
The first character must be alphabetic

@item
There can not be two punctuation marks (or spaces) in a row

@item
There can not be three of the same character in a row

@item
There can not be more than three digits

@item
Digits must appear only at the end -- i.e., if there are any digits, the
leftmost digit must be after the rightmost non-digit character.

@end itemize"
  (and (stringp Toot-name)
       (<= 3 (length Toot-name) 32)
       (every #'potential-Toot-name-character-p
              Toot-name)
       (alpha-char-p (char Toot-name 0))
       (not (three-chars-in-a-row-p Toot-name))
       (not (two-chars-in-a-row-p Toot-name #(#\Space #\Apostrophe
                                              #\Hyphen-Minus)))
       (or (notany #'digit-char-p Toot-name)
           (< (position-if (complement #'digit-char-p) Toot-name :from-end t)
              (position-if #'digit-char-p Toot-name)))
       (< (count-if #'digit-char-p Toot-name) 4)))

(deftype Toot-name ()
  `(and string (satisfies potential-Toot-name-p)))



;;; HTTP Request Methods (aka verbs)

(deftype http-request-method ()
  "All HTTP request methods (aka verbs) defined in an IETF RFC."
  '(member :get :head :post :put :delete :trace :options :connect :patch))

(deftype http-safe-request-method ()
  "HTTP request methods that make no changes, so can be replayed ad infinitum."
  '(member :get :head :options :trace))

(deftype http-idempotent-request-method ()
  "HTTP request methods which, if replayed, do no harm, but may yield an
harmless  error   message  on  the  second   and  subsequent  attempts."
  '(member :get :head :options :trace :put :delete))

;;; Conditions  that are  returned to  be  handled by  the client;  i.e.
;;; these are conditions that translate directly into HTTP errors.

(deftype http-response-success-status-number ()
  '(member 100 101
    200 201 202 203 204 205 206 207
    300 301 302 303 304 305 307))

(deftype http-response-failure-status-number ()
  '(member
    400 401 402 403 404 405 406 407 408 409
    410 411 412 413 414 415 416 417 424 428 429 431
    500 501 502 503 504 505 511))

(deftype http-response-status-number ()
  '(or http-response-success-status-number
    http-response-failure-status-number))

(define-condition http-client-error (error)
  ((http-status-code :type http-response-failure-status-number
                     :reader http-status-code)))

(defun pretty-print-html-error (condition)
  "Produces an HTML page explaining CONDITION.

TODO: Use templates, filter backtrace like Rollbar, do better."
  (format nil "<!DOCTYPE html><html><head>
<meta charset=\"utf-8\">
<title> Error ~D — Tootsville</title>
<link rel=\"stylesheet\"
      href=\"https://www.tootsville.org/error/simple-error.2017.css\">
</head>
<body>
<h2> Error ~:*~D </h2>
<h1> ~A </h1>
<ul>
<li>
  <a href=\"http://wiki.tootsville.org/wiki/Error_~0@*~D\">More info…</a>~*
</li>
<li>
 <a href=\"http://~a/\">~:*~a</a>
</li>
</ul>
<pre>~A</pre>
~@[~:*<dl>
~{<dt> ~a </dt> <dd> ~a </dd> ~}
</dl>~]
</body>
</html>"
          (http-status-code condition)
          (if hunchentoot:*show-lisp-errors-p*
              condition
              (gethash (http-status-code condition) *http-status-message*))
          (cluster-name)
          (if hunchentoot:*show-lisp-backtraces-p*
              (trivial-backtrace:backtrace-string)
              "More information is in the server logs")
          (if hunchentoot:*show-lisp-backtraces-p*
              (mapcar
               (lambda (restart)
                 (list restart (princ-to-string restart))); TODO report?
               (compute-restarts condition))
              nil)))

(define-condition not-your-Toot-error (http-client-error)
  ((http-status-code :initform 404)
   (name :initarg name :accessor which-Toot-is-not-yours))
  (:report (lambda (c s)
             (format s "You do not have a Toot named “~a.”"
                     (which-Toot-is-not-yours c)))))

(define-condition unimplemented (http-client-error)
  ((http-status-code :initform 501)
   (feature :initarg :feature :accessor unimplemented-feature
            :initform "The feature you tried to access"))
  (:report (lambda (c s)
             (format s "~a has not been implemented." (unimplemented-feature c))))
  (:documentation "Signals that a feature has not been inmplemented yet"))

(define-condition not-found (http-client-error)
  ((http-status-code :initform 404)
   (thing :initarg :thing :initarg :object :initarg :item :initarg :the
          :initform "The requested object"
          :accessor not-found-thing))
  (:report (lambda (c s)
             (format s "~a was not found." (not-found-thing c)))))

(define-condition gone (not-found)
  ((http-status-code :initform 402))
  (:report (lambda (c s)
             (format s "~a is gone." (not-found-thing c)))))



(define-constant +Toot-base-color-names+
    '(Cyan Indigo Orange Pink Red Turquoise Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot base colors")

(define-memo-function Toot-base-color-name-p (string-designator)
  (check-type string-designator string-designator)
  (member string-designator +Toot-base-color-names+
          :test #'string-equal))

(deftype Toot-base-color-name ()
  '(and string-designator
    (satisfies Toot-base-color-name-p)))

(define-constant +Toot-pads-color-names+
    '(Cyan Indigo Pink Red "Spring Green" Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot pads colors")

(define-memo-function Toot-pads-color-name-p (string-designator)
  "Is STRING-DESIGNATOR a color that can be used for foot pads and trunk tips"
  (check-type string-designator string-designator)
  (member string-designator +Toot-pads-color-names+
          :test #'string-equal))

(deftype Toot-pads-color-name ()
  "A color name that can be used for Toot foot pads and nose tip"
  '(and string-designator
    (satisfies Toot-pads-color-name-p)))

(define-constant +Toot-pattern-color-names+
    '(Black Cyan Indigo Orange Pink Rainbow Turquoise Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot pattern colors")

(define-memo-function Toot-pattern-color-name-p (string-designator)
  "Is STRING-DESIGNATOR the name of a color that can be used for a pattern?"
  (check-type string-designator string-designator)
  (member string-designator
          +Toot-pattern-color-names+
          :test #'string-equal))

(deftype Toot-pattern-color-name ()
  "The name of a color that can be used for a pattern"
  '(and string-designator
    (satisfies Toot-pattern-color-name-p)))

(define-constant +Toot-basic-pattern-names+
    '(Flowers Horseshoes Lightning Patches "Polka Dots"
      Sparkles Spots Stars Swirls)
  :test #'equalp
  :documentation "Basic patterns available to any Toot")

(define-constant +Toot-extended-pattern-names+
    '(Circuitboard Peace)
  :test #'equalp
  :documentation "Extended patterns that require special effort to obtain")

(define-memo-function Toot-pattern-name-p (string-designator)
  "Is STRING-DESIGNATOR the name of a Toot pattern?"
  (check-type string-designator string-designator)
  (member string-designator (concatenate 'list
                                         +Toot-basic-pattern-names+
                                         +Toot-extended-pattern-names+)
          :test #'string-equal))

(deftype Toot-pattern-name ()
  "The name of a Toot pattern"
  '(and string-designator
    (satisfies Toot-pattern-name-p)))

(define-memo-function allowed-base-colors-under-pattern (pattern-color)
  (remove-if (curry #'equalp pattern-color) +Toot-base-color-names+))

(define-memo-function  allowed-pattern-colors-on-base (base-color)
  (remove-if (curry #'equalp base-color) +Toot-pattern-color-names+))

(defun check-pattern-on-base-color (pattern-color base-color
                                    &key Toot-name pads-color pattern address)
  (tagbody do-over
     (restart-case
         (progn
           (check-type base-color Toot-base-color-name "The name ofa Toot base color")
           (check-type pattern-color Toot-pattern-color-name "The name of a Toot pattern color")
           (when (equal pattern-color base-color)
             (error "A Toot may not have the same base and pattern color; currently, both are ~:(~a~)~
~@[, with a pattern of ~:(~a~)~]~
~@[, and pads color of ~:(~a~)~]~
~@[ for the Toot named ~a~]~
~@[ (player with e-mail ~a)~]."
                    base-color pattern pads-color Toot-name address))
           (list pattern-color base-color))
       (change-pattern-color (color)
         :report (lambda (s)
                   (format s "Change the pattern color to one of: ~:(~{~a~^, ~}~)"
                           (allowed-pattern-colors-on-base base-color)))
         (setf pattern-color color)
         (check-type pattern-color Toot-pattern-color-name)
         (go do-over))
       (change-base-color (color)
         :report (lambda (s)
                   (format s "Change the base color to one of: ~:(~{~a~^, ~}~)"
                           (allowed-base-colors-under-pattern pattern-color)))
         :interactive-function (lambda (s)
                                 (let ((bases (allowed-base-colors-under-pattern pattern-color)))
                                   (format s "~&Choose a new base color by name or number:")
                                   (loop for color in bases
                                      for i from 1
                                      do (format s "~% [ ~d ] ~:(~a~)" i color))
                                   (format s "~% Base Color 🢩 ")
                                   (finish-output s)
                                   (let ((in (read s)))
                                     (typecase in
                                       (number
                                        (assert (<= 1 in (length bases)) (in)
                                                "The number ~d is not ~
the index from 1 to ~d of a new base color in the list where 1=~{~a~^, ~}"
                                                in (length bases) bases)
                                        (elt bases in))
                                       (t (princ-to-string in))))))
         (setf base-color color)
         (check-type base-color Toot-pattern-color-name)
         (go do-over)))))



(defun host-name-char-p (char)
  "Is CHAR a constituent character that could be in a DNS host name?"
  (check-type char character)
  (or (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (char<= #\0 char #\9)
      (char= #\. char)
      (char= #\- char)))

(defun host-name-like-p (name)
  "Does NAME meet the general rules of being a DNS host name.

 TODO: Compare this against RFCs for DNS names."
  (check-type name string)
  (and (every #'host-name-char-p name)
       (not (char= #\- (char name 0)))
       (not (char= #\- (char name (1- (length name)))))
       (not (two-chars-in-a-row-p name ".-"))
       (let ((tld (subseq name (1+ (position #\. name :from-end t)))))
         (and (every #'alpha-char-p tld)
              (<= 2 (length tld))))))

(assert (host-name-like-p "tootsville.org"))
(assert (host-name-like-p "www.tootsvillle.org"))
(assert (host-name-like-p "www.gov.uk"))
(assert (host-name-like-p "s3.amazonaws.com"))

(defun www-uri-like-p (uri)
  "Does URI look like a WWW (HTTP/HTTPS) URI?"
  (check-type uri string)
  (and (<= 3 (count #\/ uri))
       (destructuring-bind (method _ host+port)
           (split-sequence #\/ uri :count 3)
         (and (emptyp _)
              (or (string= "https:" method)
                  (string= "http:" method))
              (host-name-like-p (subseq host+port
                                        0
                                        (position #\: host+port)))))))

(assert (www-uri-like-p "https://www.tootsville.org/"))
(assert (www-uri-like-p "https://users.tootsville.org/users/foo/bar?blah=%49"))
(assert (www-uri-like-p "https://s3.amazonaws.com:443/echo.api/echo-api-cert.pem"))

(deftype dns-name ()
  '(and string (satisfies host-name-like-p)))

(deftype www-uri ()
  '(and string (satisfies www-uri-like-p)))


(defun string-length-2-p (s)
  (check-type s string)
  (equal 2 (length s)))

(defun string-all-alpha-chars-p (s)
  (check-type s string)
  (every #'alpha-char-p s))

(deftype two-letter-string ()
  '(and string
    (satisfies string-length-2-p)
    (satisfies string-all-alpha-chars-p)))


(defun integer-to-byte-vector
    (integer
     &optional
       (vector (make-array (ceiling (integer-length integer) 8)
                           :element-type '(unsigned-byte 8))))
  "Convert INTEGER into VECTOR of (UNSIGNED-BYTE 8)

If VECTOR is  supplied, it must be lon enough  to accept INTEGER without
growing. Otherwise,  the vector  of the minimum  length to  hold INTEGER
will be constructed.

The byte vector will be in big-endian (aka \"network\") order."
  (assert (<= (ceiling (integer-length integer) 8)
              (length vector))
          (integer vector)
          "INTEGER-TO-BYTE-VECTOR: ~
integer ~x is longer than vector length ~:d byte~p"
          integer (length vector))
  (let ((i8 (* 8 (1- (length vector)))))
    (dotimes (i (length vector))
      (setf (aref vector i) (ldb (byte 8 i8) integer))
      (decf i8 8)))
  vector)

(defun byte-vector-to-integer (vector)
  "Convert VECTOR of (UNSIGNED-BYTE 8) into an integer.

The VECTOR should be in big-endian (aka \"network\") order."
  (let ((i8 (* 8 (1- (length vector)))) (integer 0))
    (dotimes (i (length vector))
      (setf (ldb (byte 8 i8) integer) (aref vector i))
      (decf i8 8))
    integer))



(defstruct color24 red green blue)

(defun color24-hsv (color)
  (declare (optimize (speed 1) (safety 2)))
  (let* ((red (the (real 0 1)
                   (/ (the (unsigned-byte 8) (color24-red color))
                      255.0d0)))
         (green (the (real 0 1)
                     (/ (the (unsigned-byte 8) (color24-green color))
                        255.0d0)))
         (blue (the (real 0 1)
                    (/ (the (unsigned-byte 8) (color24-blue color))
                       255.0d0)))
         (c-max (the (real 0 1) (max red green blue)))
         (c-min (the (real 0 1) (min red green blue)))
         (delta (the (real 0 1) (- (the (real 0 1) c-max)
                                   (the (real 0 1) c-min)))))
    (if (< 0 delta)
        (list
         ;; hue
         (mod (* (/ (* 60.0d0
                       (cond
                         ((= c-max red) (mod (/ (- green blue) delta) 6))
                         ((= c-max green) (+ (/ (- blue red) delta) 2))
                         ((= c-max blue) (+ (/ (- red green) delta) 4))
                         (t (error "unreachable"))))
                    360.0d0)
                 2 pi)
              (* 2 pi))
         ;; saturation
         (if (< 0 c-max)
             (/ delta c-max)
             0)
         ;; value
         c-max)
        ;; else
        (list 0 0 c-max))))

(defun color24-hue (color)
  (first (color24-hsv color)))

(defun color24-saturation (color)
  (second (color24-hsv color)))

(defun color24-value (color)
  (third (color24-hsv color)))

(defun integer-to-color24 (number)
  (make-color24 :red (ldb (byte 8 16) number)
                :green (ldb (byte 8 8) number)
                :blue (ldb (byte 8 0) number)))

(defun color24-to-integer (color)
  (+ (ash (color24-red color) 16)
     (ash (color24-green color) 8)
     (color24-blue color)))


(defun legal-age (date-of-birth &optional (reference-date (local-time:now)))
  "The age of  a person born on DATE-OF-BIRTH, as  of REFERENCE-DATE (or
right  now);  this uses  the  legal  definition  that the  person's  age
increments at  the midnight of their  date of birth each  year, with the
date 29 February treated as 1 March on non-leap-years.

The time  zone used for  this computation  is the not  defined, however,
yielding  rather  irregular  behaviour   depending  on  time  zones  and
the like.

TODO: Determine  in what  time zone  we should  computer this  for legal
reasons, eg, COPPA."
  (check-type date-of-birth timestamp)
  (check-type reference-date timestamp)
  (unless (timestamp< date-of-birth reference-date)
    (return-from legal-age 0))
  (multiple-value-bind (msec sec min hour day month year)
      (decode-timestamp reference-date)
    (declare (ignore msec sec min hour))
    (multiple-value-bind (msec sec min hour
                               day-of-birth month-of-birth year-of-birth)
        (decode-timestamp date-of-birth)
      (declare (ignore msec sec min hour))
      (let ((had-birthday-p (or (< month-of-birth month)
                                (and (= month-of-birth month)
                                     (<= day-of-birth day)))))
        (+ (- year
              year-of-birth
              1)
           (if had-birthday-p 1 0))))))
