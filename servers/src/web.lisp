(in-package :Tootsville)



(defun accepts-content-type-p (content-type)
  "Does the current Hunchentoot request Accept: CONTENT-TYPE?"
  (etypecase content-type
    (string (string-equal content-type (hunchentoot:header-in* :accept)))
    (cons (member (hunchentoot:header-in* :accept)
                  content-type
                  :test #'string-equal))))



(defun wants-json-p ()
  "Does the client request Accept JSON format?

Looks   for   some    odd   synonyms   as   well    as   the   canonical
\"application/json\", and also checks the request URI for \".js\" (which
is, of course, a subseq of \".json\" as well.)"
  (let ((accept (hunchentoot:header-in* :accept)))
    (or (search "application/json" accept)
        (search "text/json" accept)
        (search "application/x-json" accept)
        (search ".js" (hunchentoot:request-uri*)))))








(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun after-slash (s)
    "Splits a string S at a slash. Useful for getting the end of a content-type."
    (if (find #\/ s)
        (subseq (string-downcase s) (1+ (position #\/ s)))
        (string-downcase s)))

  (defmacro check-arg-type (arg type &optional name)
    "Ensure that ARG  is of type TYPE, which is  called NAME. Signals back
to an HTTP client with a 400 error if this assertion is untrue.

This is basically just CHECK-TYPE for arguments passed by the user."
    `(unless (typep ,arg ',type)
       (return-from endpoint
         (list 400
               (list :content-type "application/json")
               (list :error
                     ,(format nil "Value provided for ~:(~a~) is not a valid ~a"
                              arg (or name (string-capitalize type)))
                     :expected-type ,(or name (string-capitalize type))
                     :argument-name ,(string-capitalize arg)
                     :provided-value(format nil "~s" ,arg))))))

  (defvar *extensions-for-content-types*
    '(
      :application/ecmascript "es"
      :application/epub+zip "epub"
      :application/java-archive "jar"
      :application/javascript "js"
      :application/json "json"
      :application/msword "doc"
      :application/octet-stream "bin"
      :application/pdf "pdf"
      :application/postscript "ps"
      :application/rtf "rtf"
      :application/tar "tar"
      :application/vnd.amazon.ebook "azw"
      :application/vnd.apple.installer+xml "mpkg"
      :application/vnd.mozilla.xul+xml "xul"
      :application/vnd.ms-fontobject "eot"
      :application/vnd.oasis.opendocument.presentation "odp"
      :application/vnd.oasis.opendocument.spreadsheet "ods"
      :application/vnd.oasis.opendocument.text "odt"
      :application/vnd.openxmlformats-officedocument.wordprocessingml.document "docx"
      :application/x-7z-compressed "7z"
      :application/x-abiword "abw"
      :application/x-bcpio "bcpio"
      :application/x-bzip "bz"
      :application/x-bzip2 "bz2"
      :application/x-compress "z"
      :application/x-cpio "cpio"
      :application/x-csh "csh"
      :application/x-gzip "gz"
      :application/x-latex "tex"
      :application/x-rar-compressed "rar"
      :application/x-sh "sh"
      :application/x-shar "shar"
      :application/x-texinfo "texi"
      :application/x-troff "roff"
      :application/x-troff-man "man"
      :application/x-troff-me "me"
      :application/x-troff-ms "ms"
      :application/xhtml+xml "xhtml"
      :application/xml "xml"
      :application/zip "zip"
      :audio/3gpp "3gp" ; * same as video, use care
      :audio/3gpp2 "3g2" ; * same as audio, use care
      :audio/aac "aac"
      :audio/basic "au"
      :audio/midi "midi"
      :audio/ogg "oga"
      :audio/wav "wav"
      :audio/webm "weba"
      :audio/x-aiff "aiff"
      :audio/x-mpegurl "m3u"
      :font/otf "otf"
      :font/ttf "ttf"
      :font/woff "woff"
      :font/woff2 "woff2"
      :image/gif "gif"
      :image/jpeg "jpg"
      :image/jpeg "jpg"
      :image/png "png"
      :image/png "png"
      :image/png "png"
      :image/svg "svg"
      :image/svg "svg"
      :image/tiff "tiff"
      :image/webp "webp"
      :image/x-icon "ico"
      :image/x-xbitmap "xbm"
      :image/x-xpixmap "xpm"
      :message/rfc822 "mbox"
      :text/calendar "ics"
      :text/css "css"
      :text/csv "csv"
      :text/html "html"
      :text/plain "txt"
      :text/richtext "rtx"
      :text/tab-separated-values "tsv"
      :text/x-vcard "vcf"
      :video/3gpp "3gp"
      :video/3gpp2 "3g2"
      :video/mp4 "mp4"
      :video/mpeg "mpeg"
      :video/ogg "ogv"
      :video/quicktime "qt"
      :video/webm "webm"
      :video/x-ms-asf "asf"
      :video/x-msvideo "avi"
      :video/x-sgi-movie "movie"
      :x-world/x-vrml "vrml"
      ))

  (defun extension-for-content-type (content-type)
    (getf *extensions-for-content-types*
          (make-keyword (string-upcase content-type))))

  (defun name-for-content-type (content-type)
    (or (extension-for-content-type content-type)
        (after-slash content-type)))

  (defun atom-or-comma-list (value)
    (cond
      ((atom value) value)
      ((= 1 (length value)) (first value))
      (t (format nil "~{~a~^, ~}" value))))

  (defmacro rewrite-restas ((&key (jsonp nil)) &body body)
 ;;; TODO, maybe should be render-method in RESTAS?
    `(destructuring-bind (status headers content)
         (progn ,@body)
       (check-type status (integer 200 599))
       (check-type headers (or null cons))
       (check-type content (or cons string array))
       (assert (every (lambda (x)
                        (or (stringp x) (symbolp x)))
                      headers)
               (headers)
               "Headers should be given as strings or symbols; got ~s"
               headers)
       (setf (hunchentoot:return-code*) status)
       (loop for (header . value) on headers by #'cddr
          do (setf (hunchentoot:header-out header)
                   (atom-or-comma-list value)))
       ,(if jsonp
            `(if (consp content)
                 (render-json content)
                 content)
            '(flexi-streams:string-to-octets content :external-format :utf-8))))

  (defun add-charset (content-type)
    "Adds the ;charset=UTF-8 type to the end of text and JS/JSON CONTENT-TYPEs"
    (if (member content-type
                '("text/plain" "text/html"
                  "application/javascript"
                  "application/json")
                :test 'string=)
        (concatenate 'string content-type "; charset=utf-8")
        content-type))

  (assert (equal (add-charset "text/html")
                 "text/html; charset=utf-8"))
  (assert (equal (add-charset "text/plain")
                 "text/plain; charset=utf-8"))
  (assert (equal (add-charset "application/javascript")
                 "application/javascript; charset=utf-8"))
  (assert (equal (add-charset "application/json")
                 "application/json; charset=utf-8"))
  (assert (equal (add-charset "image/png")
                 "image/png"))

  (defun constituentp (ch)
    (let ((cc (char-code (char-upcase ch))))
      (or (< #xa0 cc)
          (<= (char-code #\A) cc (char-code #\Z))
          (<= (char-code #\0) cc (char-code #\9))
          (find ch "-/!?." :test #'char=))))

  (defun make-endpoint-function-name (method uri &optional accept-type)
    (intern (format nil "ENDPOINT-~a-~a~@[->~a~]"
                    method
                    (remove-if-not #'constituentp uri)
                    (cond
                      ((null accept-type) nil)
                      ((stringp accept-type)
                       (name-for-content-type accept-type))))))

  (defun lambda-list-as-variables (λ-list)
    (if λ-list
        (cons 'list (mapcar (lambda (var)
                              (list 'quote var))
                            λ-list))
        'nil))
  
  (defun defendpoint/make-extension-named-route (fname λ-list
                                                 method uri extension)
    (let ((typed-uri (concatenate 'string 
                                  uri (if (char= #\/ (last-elt uri))
                                          "index."
                                          ".")
                                  extension)))
      `(restas::register-route-traits
        ',fname
        (plist-hash-table (list :template ,typed-uri
                                :method ,method
                                :variables ,(lambda-list-as-variables λ-list))))))
  
  (defmacro defendpoint ((method uri &optional content-type)
                         &body body)
    (let* ((method (make-keyword (symbol-name method)))
           (fname (make-endpoint-function-name method uri content-type))
           (content-type (string-downcase content-type))
           (λ-list (mapcar (compose #'intern #'symbol-name)
                           (routes:template-variables
                            (routes:parse-template uri))))
           (docstring (if (and (consp body) (stringp (first body)))
                          (first body)
                          (format nil
                                  "Endpoint for URI's matching the pattern ~s~
~@[ and accepting content-type ~a~]"
                                  uri content-type))))
      `(progn
         (defun ,fname (,@λ-list) ,docstring
                (v:info '(,(make-keyword fname) :endpoint :endpoint-start) ,(concatenate 'string "{~a} Starting: " docstring)
                        (thread-name (current-thread)))
                ,(unless (consp content-type)
                   `(setf (hunchentoot:content-type*)
                          ,(add-charset content-type)))
                (let ((content-bytes
                       (rewrite-restas (:jsonp ,(equal content-type "application/json"))
                         (catch 'endpoint-over
                           (block endpoint
                             (block ,fname
                               ,@body))))))
                  (v:info '(,(make-keyword fname) :endpoint :endpoint-finish) ,(concatenate 'string "{~a} Finished: " docstring)
                          (thread-name (current-thread)))
                  (v:info '(,(make-keyword fname) :endpoint :endpoint-output) "{~a} Status: ~d; ~[~:;~:*~d header~:p; ~]~d octets"
                          (thread-name (current-thread))
                          (hunchentoot:return-code*)
                          (length (hunchentoot:headers-out*))
                          (length content-bytes))
                  content-bytes))
         (restas::register-route-traits
          ',fname
          (plist-hash-table
           (list :template ,uri
                 :method ,method
                 :content-type ,content-type
                 :variables ,(lambda-list-as-variables λ-list))))
         ,(when-let (extension (extension-for-content-type content-type))
            (defendpoint/make-extension-named-route
                fname λ-list method uri extension))
         (restas:reconnect-all-routes)))))

(defendpoint (get "/" text/html)
  (list 307 '(:location "https://Tootsville.org/") ""))



(defmethod print-object ((route restas:route) stream)
  (print-unreadable-object (route stream :type t)
    (princ (string-capitalize (restas:route-symbol route)) stream)
    (write-char #\Space stream)
    (format stream "~{~:(~a~): ~a~^ ~}" (restas::route-headers route))
    (write-char #\Space stream)

    (write-char #\Space stream)
    (princ (restas::route-arbitrary-requirement route) stream)))


(defmethod print-object ((vhost restas::vhost) stream)
  (print-unreadable-object (vhost stream :type t)
    (destructuring-bind (hostname . port) (restas::vhost-hostname-port vhost)
      (princ hostname stream)
      (write-char #\: stream)
      (princ port stream))))

(defmethod print-object ((request hunchentoot:request) stream)
  (print-unreadable-object (request stream :type t)
    (princ (hunchentoot:request-method request) stream)
    (write-char #\Space stream)
    (princ (hunchentoot:request-uri request) stream)))

(defmethod print-object ((template routes:uri-component-template) stream)
  (print-unreadable-object (template stream :type t)
    (princ (slot-value template 'routes::spec) stream)))

(defmethod print-object ((template routes:or-template) stream)
  (print-unreadable-object (template stream :type t)
    (format stream "~{~a~^ or ~}" (slot-value template 'routes::spec))))
