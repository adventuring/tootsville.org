(defpackage com.twilio
  (:use :cl)
  (:export ))

(in-package :com.twilio)



;;; This   software   is  based   upon   the   Twilio  Ruby   API   from
;;; https://github.com/twilio/twilio-ruby  and  therefore  inherits  its
;;; license:
;;;
;;; 
;;; MIT License
;;;
;;; Copyright © 2018, Twilio, Inc. help@twilio.com
;;;
;;; Permission  is  hereby  granted,  free  of  charge,  to  any  person
;;; obtaining a copy of this software and associated documentation files
;;; (the  "Software"),  to deal  in  the  Software without  restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to  permit persons to whom  the Software is furnished  to do so,
;;; subject to the following conditions:
;;;
;;; The  above copyright  notice  and this  permission  notice shall  be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE  SOFTWARE IS  PROVIDED "AS  IS", WITHOUT  WARRANTY OF  ANY KIND,
;;; EXPRESS OR IMPLIED,  INCLUDING BUT NOT LIMITED TO  THE WARRANTIES OF
;;; MERCHANTABILITY,    FITNESS   FOR    A   PARTICULAR    PURPOSE   AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL  THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER IN AN
;;; ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN
;;; CONNECTION  WITH  THE SOFTWARE  OR  THE  USE  OR OTHER  DEALINGS  IN
;;; THE SOFTWARE.



;;; framework/domain.rb

(defclass rest-domain ()
  ((client :accessor rest-domain-client
           :initarg :client 
           :initform (error ":CLIENT required"))
   (host)
   (base-url)
   (port)))

(defmethod absolute-url ((domain rest-domain) uri)
  (with-slots (base-url) domain
    (concatenate 'string
                 (string-trim "/" base-url)
                 "/"
                 (string-trim "/" uri))))

(defmethod request ((domain rest-domain) method uri
                    &rest key-args
                    &key params data headers auth timeout)
  (let ((url (if (string= "http" uri :end2 4)
                 uri
                 (absolute-url domain uri))))
    (with-slots (host port client) domain
      (apply #'http-request 
             client host port method url
             key-args))))



;;; framework/error.rb

(define-condition twilio-error (error))

(define-condition rest-error (twilio-error)
  ((message :accessor error-message :initarg :message)
   (response :accessor error-response :initarg :response)
   (code :accessor error-code :initarg :code)
   (status-code :accessor error-status-code :initarg :status-code)
   (detail :accessor error-detail :initarg :detail)
   (more-info :accessor error-more-info :initarg :more-info)
   (error-message :accessor error-error-message :initarg :error-message)))

(defun make-rest-error (message response)
  (let ((status-code (response-status-code response))
        (body (response-body response)))
    (let ((error (make-instance 'rest-error
                                :status-code status-code
                                :code (extract body "code" status-code)
                                :detail (extract body "detail")
                                :error-message (extract body "message")
                                :more-info (extract body "more-info")
                                :response response)))
      (setf (error-message error) (format-message error message))
      error)))

(defmethod print-object ((error twilio-error) s)
  (princ (error-message error) s))

(defun format-message (error initial-message)
  (labels ((maybe (object)
             (if object (list #(#\Newline) object) "")))
    (with-slots (status-code code error-message detail more-info) error
      (apply #'concatenate 'string
             "[HTTP "
             (princ-to-string status-code)
             "] "
             (princ-to-string error)
             " : "
             initial-message
             (maybe error-message)
             (maybe detail)
             (maybe more-info)
             '(#(#\Newline #\Newline))))))

(define-condition obsolete-error (error))



;;; framework/helper.rb

(defun url-join (left right)
  (concatenate 'string
               (string-trim "/" left)
               "/"
               (string-trim "/" right)))



;;; framework/obsolete-client.rb → not needed



;;; framework/page.rb

(defclass page ()
  ((version :accessor page-version :initarg :version)
   (payload :accessor page-payload :initarg :payload)
   (solution :accessor page-solution :initarg :solution)
   (records :accessor page-records :initarg :records)))

(define-constant meta-keys
    '("end"
      "first_page_uri"
      "next_page_uri"
      "page"
      "page_size"
      "previous_page_uri"
      "total"
      "num_pages"
      "start"
      "uri")
  :test 'equalp)

(defun meta-key-p (string)
  (member string meta-keys :test 'string-equal))

(defun make-page (version response)
  (let ((payload (process-response response)))
    (make-instance 'page
                   :version version
                   :payload payload
                   :solution nil
                   :records (load-page payload))))

(defun process-response (response)
  (unless (= 200 (response-status-code response))
    (error (make-rest-error "Unable to fetch page" response))))

(define-condition can-not-deserialize-page-error (twilio-error))

(defmethod print-object ((error can-not-deserialize-page-error) s)
  (princ  "Page Records can not be deserialized" s))

(defun load-page (payload)
  (if-let (key (and (extract payload "meta")
                    (extract payload "meta" "key")))
    (extract payload key)
    (let* ((keys (extract payload "keys"))
           (key (remove-if #'meta-key-p keys)))
      (if (= 1 (length key))
          (car key)
          (error 'can-not-deserialize-page-error)))))

(defmacro define-other-page (direction)
  (let ((key (concatenate 'string (string-downcase direction) "_page_url"))
        (url-fn (intern (concatenate 'string (string direction) #:-page-url)))
        (go-fn (intern (concatenate 'string (string direction) #:-page-url)))))
  `(progn 
     (defun ,url-fn (page)
       (let ((payload (page-payload page)))
         (if-let (url (and (extract payload "meta") 
                           (extract payload "meta" ,key)))
           (absolute-url (version-domain (page-version page)) url)
           (when-let ((url (extract payload ,key)))
             (absolute-url (version-domain (page-version page)) url)))))
     (defun ,go-fn (page)
       (when-let (url (,url-fn page))
         (let ((response (request (version-domain page) "GET" url)))
           (make-instance (class-name (class-of page))
                          :version version
                          :response response
                          :solution solution))))))

(define-other-page previous)
(define-other-page next)

(defmacro map-records ((var page) &body body)
  (let ((record (gensym "RECORD-")))
    `(dolist (,record (page-records ,page))
       (let ((,var (get-instance ,page ,record)))
         ,@body))))



;;; framework/request.rb

(defclass request ()
  ((host :accessor request-host :initarg :host)
   (port :accessor request-port :initarg :port)
   (method :accessor request-method :initarg :method)
   (url :accessor request-url :initarg :url)
   (params :accessor request-params :initarg :params)
   (data :accessor request-data :initarg :data)
   (headers :accessor request-headers :initarg :headers)
   (auth :accessor request-auth :initarg :auth)
   (timeout :accessor request-timeout :initarg :timeout)))

(defmethod print-object ((request request) s)
  (with-slots (auth method url params headers data) request
    (format s "~@[(~{~a~^,~}) ~]~
~a ~a~
~@[?~{~/cgi-escape/:~/cgi-escape/~^&~} ~]~
~%~@[ -G~]
~@[~{-d \"~a\"=\"~a\"~^~%~}~]~
~@[~%~{-H \"~a\"=\"~a\"~^~%~}~]" 
            auth method url
            params
            (equal "GET" method)
            data
            headers)))



;;; framework/response.rb

(defclass response ()
  ((status-code :accessor response-status-code :initarg :status-code)
   (body :accessor response-body :initarg :body) 
   (headers :accessor response-headers :initarg :headers)))

(defun make-response (status-code body &optional headers)
  (make-instance 'response
                 :status-code status-code
                 :body (and body
                            (jonathan:parse body))
                 :headers headers))

(defmethod print-object ((response response) s)
  (with-slots (status-code body) response
    (format s "[~d] ~a" status-code body)))



;;; framework/serialize.rb

(defgeneric serialize-iso-8601-date (date)
  (:method ((date (eql :unset)))
    date)
  (:method ((date timestamp))
    (format-timestring nil date +iso-8601-date-format+))
  (:method ((date string))
    date))

(defgeneric serialize-iso-8601-date+time (date+time)
  (:method ((date+time (eql :unset)))
    date+time)
  (:method ((date+time timestamp))
    (format-timestring nil date+item +iso-8601-format+))
  (:method ((date+time string))
    date+time))

(defun deserialize-rfc-2822 (date)
  (error "Unimplemented. Do we need it?"))

(defun deserialize-iso-8601 (date)
  (parse-timestring date))

(defun serialize-object (object)
  (jonathan:to-json object))

(defun flatten-plist (plist &optional result previous)
  (loop for (key value) on plist by #'cddr
     do (if (plist-p value)
            (flatten-plist value result (append previous (list key)))
            (setf (getf result (format-symbol :keyword "~{~a.~}~a" previous key)) value))
     finally (return result)))

(defgeneric prefixed-collapsible-plist (object prefix)
  (:method (object _)
    (declare (ignore _))
    object)
  (:method ((plist proper-list) prefix)
    (let (result
          (flattened (flatten-plist plist)))
      (loop for (key value) on flattened by #'cddr
         do (setf (getf result (format-symbol :keyword "~a.~a" prefix key)) value)))))

(defgeneric serialize-list (object)
  (:method (object)
    object)
  (:method ((vector vector))
    (loop for element on vector collecting element))

