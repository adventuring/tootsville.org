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




(define-condition unimplemented (error)
  ()
  (:documentation "Signals that a feature has not been inmplemented yet"))



;; Error pages — Legacy CAVEMAN2 handler …
#+ (or)
(defmethod on-exception ((app <Tootsville>) code)
  "Return error with code CODE

CODE is allowed to be a string beginning with an HTTP error code.

CODE must be between 300-599, inclusive, or 501 will be used.

TODO: We SHOULD validate that CODE is a sane HTTP error code, but we don't."
  (declare (ignore app))
  (cond
    ((consp code)
     (render-json code))
    ((wants-json-p)
     (render-json `((:error . ,code))))
    (t (let ((code-number (typecase code
                            (number code)
                            (string (parse-integer code :junk-allowed t))
                            (caveman2.exception:http-exception
                             (caveman2.exception:exception-code code))
                            (t 501))))
         (unless (<= 300 code-number 599)
           (setf code-number 501))
         (redirect-to (format nil "https://www.Tootsville.org/error/~d.shtml" code-number))))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun after-slash (s)
    (if (find #\/ s)
        (subseq (string-downcase s) (1+ (position #\/ s)))
        (string-downcase s))))

(defmacro check-arg-type (arg type &optional name)
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

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun add-charset (content-type)
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

  (defun make-endpoint-function-name (method uri accept-type)
    (intern (string-upcase
             (format nil "ENDPOINT-~a-~a~@[->~a~]"
                     method
                     (remove-if-not #'constituentp uri)
                     (cond
                       ((null accept-type) nil)
                       ((consp accept-type)
                        (format nil "~{~a~^/~}"
                                (mapcar #'name-for-content-type accept-type)))
                       ((stringp accept-type)
                        (name-for-content-type accept-type)))))))
  
  (defun lambda-list-as-variables (λ-list)
    (if λ-list
        (cons 'list (mapcar
                         (lambda (x) (list 'quote x))
                         λ-list))
        'nil))

  (defun defendpoint/make-extension-named-route (fname λ-list
                                                 method uri extension)
    (let ((typed-uri (concatenate 'string uri "." extension)))
      `((restas::register-route-traits
         ',fname
         (plist-hash-table (list :template ,typed-uri
                                 :method ,method
                                 :variables ,(lambda-list-as-variables λ-list)))))))

  (defmacro defendpoint ((method uri &optional accept-type)
                         &body body)
    (let* ((method (make-keyword (symbol-name method)))
           (fname (make-endpoint-function-name method uri accept-type))
           (accept-types (ensure-list accept-type))
           (λ-list (mapcar (compose #'intern #'symbol-name)
                           (routes:template-variables
                            (routes:parse-template uri))))
           (docstring (if (and (consp body) (stringp (first body)))
                          (first body)
                          (format nil
                                  "Endpoint for URI's matching the pattern ~s~
~@[ and accepting content-type ~{~a~^ or ~}~]"
                                  uri accept-types))))
      `(progn
         (defun ,fname (,@λ-list) ,docstring
           ,(unless (consp accept-type)
              `(setf (hunchentoot:content-type*)
                     ,(add-charset accept-type)))
           (rewrite-restas (:jsonp ,(equal accept-type "application/json"))
             (block endpoint
               (block ,fname
                 ,@body))))
         ,@(mapcar
                (lambda (content-type)
                  `(restas::register-route-traits
                    ',fname
                    (plist-hash-table
                     (list :template ,uri
                           :method ,method
                           :content-type ,content-type
                           :variables ,(lambda-list-as-variables λ-list)))))
                accept-types)
         ,@(mapcan
            (lambda (content-type)
              (when-let (extension (extension-for-content-type content-type))
                (defendpoint/make-extension-named-route
                    fname λ-list method uri extension)))
            (ensure-list accept-type))
         (restas:reconnect-all-routes)))))

(defendpoint (get "/")
  (list 307 '(:location "https://Tootsville.org/") ""))
