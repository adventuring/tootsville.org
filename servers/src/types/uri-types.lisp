(in-package :Tootsville)

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

Note that this  does NOT recognize etiher dotted-quad IPv4  nor hex IPv6
addresses, only DNS names.

 RFC-1035:

@itemize

@item
Each label is up to 63 character-bytes.

@item
The total name length is up to 255 character-bytes, excluding dots.

@item
Labels must begin with a basic ASCII letter A-Z

@item
Labels must end with a letter or digit 0-9

@item
Labels  may contain  ASCII Hyphen-Minus,  but only  internally and
never twice in a row.

@item
At present,  all Top-Level  Domains are  at least  two alphabetic
characters and contain no digits nor hyphens.

@item
This function requires at least one dot; i.e. it is not for TLDs

@item
The trailing dot for the root should be omitted for this function.

@end itemize"
  (check-type name string)
  (and (every #'host-name-char-p name)
       (find #\. name)
       (not (char= #\- (char name 0)))
       (not (digit-char-p (char name 0)))
       (not (char= #\- (char name (1- (length name)))))
       (not (some (lambda (d)
                    (search d name))
                  '(".0" ".1" ".2" ".3" ".4"
                    ".5" ".6" ".7" ".8" ".9"
                    ".-")))
       (not (two-chars-in-a-row-p name ".-"))
       (let ((tld (subseq name (1+ (position #\. name :from-end t)))))
         (and (every #'alpha-char-p tld)
              (<= 2 (length tld))))))

(defpost host-name-like-tootsville.org ()
  (host-name-like-p "tootsville.org"))
(defpost host-name-like-star-hope.org ()
  (host-name-like-p "star-hope.org"))
(defpost host-name-like-www.tootsville.org ()
  (host-name-like-p "www.tootsvillle.org"))
(defpost host-name-like-www.gov.uk ()
  (host-name-like-p "www.gov.uk"))
(defpost host-name-like-s3.amazonaws.com ()
  (host-name-like-p "s3.amazonaws.com"))
(defpost not-host-name-like-한굴.ko ()
  (not (host-name-like-p "한굴.ko")))
(defpost not-host-name-like--foo.com ()
  (not (host-name-like-p "-foo.com")))
(defpost not-host-name-like-foo--foo.com ()
  (not (host-name-like-p "foo--foo.com")))
(defpost not-host-name-like-foo-.com ()
  (not (host-name-like-p "foo-.com")))
(defpost not-host-name-like-9foo.com ()
  (not (host-name-like-p "9foo.com")))
(defpost not-host-name-like-bar.-foo.com ()
  (not (host-name-like-p "bar.-foo.com")))
(defpost not-host-name-like-bar.9foo.com ()
  (not (host-name-like-p "bar.9foo.com")))
(defpost not-host-name-like-foo.12 ()
  (not (host-name-like-p "foo.12")))
(defpost not-host-name-like-foo.x ()
  (not (host-name-like-p "foo.x")))
(defpost not-host-name-like-foo ()
  (not (host-name-like-p "foo")))
(defpost not-host-name-like-10.0.0.10 ()
  (not (host-name-like-p "10.0.0.10")))



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

(defpost good-uri-tootsville.org ()
  (www-uri-like-p "https://www.tootsville.org/"))
(defpost good-uri-with-query-string ()
  (www-uri-like-p "https://users.tootsville.org/users/foo/bar?blah=%49"))
(defpost good-uri-amazon-s3 ()
  (www-uri-like-p "https://s3.amazonaws.com:443/echo.api/echo-api-cert.pem"))

(deftype dns-name ()
  '(and string (satisfies host-name-like-p)))

(deftype www-uri ()
  '(and string (satisfies www-uri-like-p)))
