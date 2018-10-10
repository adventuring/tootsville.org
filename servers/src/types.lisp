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

(defun check-Toot-name (name)
  "Check if NAME is allowed as a Toot name; offering restarts to correct it, if not.

This is generally intended for accepting new Toot names, versus validating REST calls, for example.")

(define-memo-function potential-Toot-name-character-p (character)
  "Is CHARACTER allowed in a Toot name at all?

Allowed characters are alphanumerics, apostrophe, hyphen, or space, but there are additional rules in `POTENTIAL-TOOT-NAME-P' which limit the string as a whole."
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
There can not be more than three of the same character in a row
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
       (not (two-chars-in-a-row-p Toot-name #(#\Space #\Apostrophe #\Hyphen-Minus)))
       (or (notany #'digit-char-p Toot-name)
           (< (position-if (complement #'digit-char-p) Toot-name :from-end t)
              (position-if #'digit-char-p Toot-name)))
       (< (count-if #'digit-char-p Toot-name) 4)))

(deftype Toot-name ()
  `(and string (satisfies potential-Toot-name-p)))



;;; HunchenToot  doesn't define  this, probably  because it's  a Twitter
;;; innovation for REST, and not an IETF RFC (yet? I think?)

(hunchenToot::def-http-return-code hunchenToot::+http-continue+ 420 "Enhance Your Calm")

;;; Conditions  that are  returned to  be  handled by  the client;  i.e.
;;; these are conditions that translate directly into HTTP errors.

(deftype http-response-success-status-number ()
  '(member 100 101 
    200 201 202 203 204 205 206 207 
    300 301 302 303 304 305 307))

(deftype http-response-failure-status-number ()
  '(member
    400 401 402 403 404 405 406 407 408 409 
    410 411 412 413 414 415 416 417 420 424 428 429 431
    500 501 502 503 504 505 511))

(deftype http-response-status-number ()
  '(or http-response-success-status-number
    htttp-response-failure-status-number))

(define-condition http-client-error (error)
  ((http-status-code :type http-response-status-number)))

(define-condition not-your-Toot-error (http-client-error)
  ((name :initarg name :accessor which-Toot-is-not-yours))
  (:report (lambda (c s)
             (format s "You do not have a Toot named “~a.”"
                     (which-Toot-is-not-yours c))))
  (:default-initargs :http-status-code hunchenToot:+http-not-found+))

(define-condition unimplemented (http-client-error)
  ()
  (:documentation "Signals that a feature has not been inmplemented yet"))



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
                                       (t (stringify in))))))
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


