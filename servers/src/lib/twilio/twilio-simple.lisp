;;;; -*- lisp -*-
;;;
;;;; ./servers/src/lib/twilio/twilio-simple.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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

(defpackage :twilio (:use :cl :cxml)
            (:export

             #:as-response
             #:with-twilio-params
             ;; ——
             #:say
             #:play
             #:play-digits
             #:dial
             #:record
             #:with-gather
             ;; ——
             #:hangup
             #:enqueue
             #:leave
             #:redirect
             #:pause
             #:reject
             ;; ——
             #:message
             ))

(in-package :twilio)



(defmacro as-response (&body body)
  `(with-xml-output (make-string-sink)
     (with-element "Response"
       ,@body)))

(defmacro with-twilio-params (() &body body)
  `(let* ((message-sid (hunchentoot:parameter "MessageSid"))
          (account-sid (hunchentoot:parameter "AccountSid"))
          (messaging-service-sid (hunchentoot:parameter "MessagingServiceSid"))
          (message-from (hunchentoot:parameter "From"))
          (message-from-user (find-person-by-url "tel://" message-from))
          (message-to (hunchentoot:parameter "To"))
          (message-body (hunchentoot:parameter "Body"))
          (message-from-place (list :city (hunchentoot:parameter "FromCity")
                                    :state (hunchentoot:parameter "FromState")
                                    :postal-code (hunchentoot:parameter "FromZip")
                                    :country (hunchentoot:parameter "FromCountry")))
          (message-to-place (list :city (hunchentoot:parameter "ToCity")
                                  :state (hunchentoot:parameter "ToState")
                                  :postal-code (hunchentoot:parameter "ToZip")
                                  :country (hunchentoot:parameter "ToCountry")))
          message-media-type
          message-media-url)
     (let ((num-media (parse-integer (hunchentoot:parameter "NumMedia"))))
       (when (< 0 num-media)
         (loop for i below num-media
            do (push (hunchentoot:parameter (format nil "MediaContentType~D" i))
                     message-media-type)
            do (push (hunchentoot:parameter (format nil "MediaUrl~D" i))
                     message-media-url)))
       ,@body)))



(defun format-language (symbol)
  (let ((name (string symbol)))
    (if (= 2 (length name))
        (string-downcase name)
        (concatenate 'string
                     (string-downcase (subseq name 0 2))
                     "-"
                     (string-upcase (subseq name 3))))))

(tootsville::defpost check-language-format-2 ()
  (assert (string= "en" (format-language :en))))

(tootsville::defpost check-language-format-5 ()
  (assert (string= "en-GB" (format-language :en-gb))))



(defun say (text &key voice loop language)
  (check-type text string)
  (check-type voice (or null (member :man :woman :alice)))
  (check-type loop (or null (and fixnum (integer 0 *))))
  (cond
    ((null voice) ; auto-selects Alice when using languages only avail in Alice
     (check-type language
                 (or null (member :en :en-gb :es :fr :de
                                  :da-dk :de-de
                                  :en-au :en-ca :en-in :en-us
                                  :ca-es :es-es :es-mx
                                  :fi-fi :fr-ca :fr-fr :it-it
                                  :ja-jp :ko-kr
                                  :nb-no :nl-nl :pl-pl
                                  :pt-br :pt-pt :ru-ru :sv-se
                                  :zh-cn :zh-hk :zh-tw
                                  ))))
    ((member voice '(:man :woman))
     (check-type language (or null (member :en :en-gb :es :fr :de))))
    ((eql voice :alice)
     (check-type language
                 (or null (member :da-dk :de-de
                                  :en-au :en-ca :en-gb :en-in :en-us
                                  :ca-es :es-es :es-mx
                                  :fi-fi :fr-ca :fr-fr :it-it
                                  :ja-jp :ko-kr
                                  :nb-no :nl-nl :pl-pl
                                  :pt-br :pt-pt :ru-ru :sv-se
                                  :zh-cn :zh-hk :zh-tw
                                  )))))
  (assert (>= 4096 (length text)))
  (with-element "Say"
    (when voice (attribute "voice" (string-downcase voice)))
    (when loop (attribute "loop" (princ-to-string loop)))
    (when language (attribute "language" (format-language language)))
    (text text)))

(defun play (uri &key loop)
  (check-type uri (or string puri:uri))
  (check-type loop (or null (integer 0 *)))
  (with-element "Play"
    (when loop (attribute "loop" (princ-to-string loop)))
    (text (etypecase uri
            (string uri)
            (puri:uri (puri:render-uri uri nil))))))

(defun play-digits (digits &key loop)
  (check-type digits string)
  (assert (every (lambda (ch)
                   (or (digit-char-p ch)
                       (char= #\w ch)))
                 digits))
  (check-type loop (or null (and fixnum (integer 0 *))))
  (with-element "Play"
    (attribute "digits" digits)
    (when loop (attribute "loop" (princ-to-string loop)))))


(defun dial (destination-number
             &key action answer-on-bridge caller-id hangup-on-star
                  method record
                  recording-status-callback
                  recording-status-callback-method
                  recording-status-callback-event
                  ring-tone time-limit timeout trim

                  client conference number queue
                  sim sip)
  (error 'tootsville::unimplemented))

(defun record (&rest _)
  (error 'tootsville::unimplemented))

(defun with-gather% (body
                     &key action hints (finish-on-key #\#)
                          input language method num-digits
                          partial-result-callback
                          partial-result-callback-method
                          (profanity-filter t profanity-filter?)
                          speech-timeout
                          timeout)
  (check-type action (or null string puri:uri))
  (check-type finish-on-key (or null
                                (and character
                                     (or (satisfies digit-char-p)
                                         (member #\* #\#)))))
  (check-type hints (or null string))
  (check-type input (or null (member :dtmf :speech :any)))
  (check-type language (or null symbol))
  (check-type method (or null (member :get :post)))
  (check-type num-digits (or null (and fixnum (integer 1 *))))
  (check-type partial-result-callback (or null string puri:uri))
  (check-type partial-result-callback-method (or null (member :get :post)))
  (check-type speech-timeout (or null (eql :auto)
                                 (and fixnum (integer 1 *))))
  (check-type timeout (or null (and fixnum (integer 1 *))))
  (with-element "Gather"
    (when action (attribute "action"
                            (etypecase action
                              (string action)
                              (puri:uri (puri:render-uri action nil)))))
    (when finish-on-key
      (attribute "finishOnKey" (princ-to-string finish-on-key)))
    (when hints (attribute "hints" hints))
    (when input (attribute "input"
                           (ecase input
                             (:dtmf "dtmf")
                             (:speech "speech")
                             (:any "dtmf speech"))))
    (when language (attribute "language" (format-language language)))
    (when num-digits (attribute "numDigits" (princ-to-string num-digits)))
    (when partial-result-callback
      (attribute "partialResultCallback"
                 (etypecase partial-result-callback
                   (string partial-result-callback)
                   (puri:uri (puri:render-uri partial-result-callback nil)))))
    (when partial-result-callback-method
      (attribute "partialResultCallbackMethod"
                 (string partial-result-callback-method)))
    (when profanity-filter?
      (attribute "profanityFilter" (if profanity-filter "true" "false")))
    (when speech-timeout
      (attribute "speechTimeout" (princ-to-string speech-timeout)))
    (when timeout (attribute "timeout" (princ-to-string timeout)))
    (funcall body)))

(defmacro with-gather ((&rest keys
                              &key action (finish-on-key #\#) hints
                              input language method num-digits
                              partial-results-callback
                              partial-results-callback-method
                              (profanity-filter t profanity-filter?)
                              speech-timeout
                              timeout)
                       &body body)
  `(with-gather% (lambda () ,@body) ,@keys))



(defun hangup ()
  (with-element "Hangup"))

(defun enqueue (&key action method wait-url wait-url-method
                     workflow-sid
                     name task)
  ;; sends additional: QueueResult QueueSid QueueTime
 ;;; QueueResult ∈ bridged,  bridging-in-progress, error, hangup, leave,
 ;;; redirected, redirected-from-bridged, queue-full, system-error
  ;; wait-url ALSO gets — AvgQueueTime, CurrentQueueSize
  (check-type action (or null string puri:uri))
  (check-type method (or null (member :get :post)))
  (check-type wait-url (or null string puri:uri))
  (check-type wait-url-method (or null (member :get :post)))
  (check-type name (or null string))
  (check-type task (or null string cons))
  (assert (or name task))
  (with-element "Enqueue"
    (when action (attribute "action" (etypecase action
                                       (string action)
                                       (puri:uri (puri:render-uri action nil)))))
    (when method (attribute "method" (string method)))
    (when wait-url (attribute "waitUrl"
                              (etypecase wait-url
                                (string wait-url)
                                (puri:uri (puri:render-uri wait-url nil)))))
    (when workflow-sid (attribute "workflowSid" workflow-sid))
    (text (or name (etypecase task
                     (string task)
                     (cons (to-json task)))))))

(defun leave ()
  (with-element "Leave"))

(defun redirect (uri &key method)
  (check-type uri (or string puri:uri))
  (check-type method (or null (member :get :post)))
  (with-element "Redirect"
    (when method
      (attribute "method" (string method)))
    (text (etypecase uri
            (string uri)
            (puri:uri (puri:render-uri uri nil))))))

(defun pause (&optional (duration 1))
  (check-type duration (integer 1 *))
  (with-element "Pause"
    (attribute "length" (princ-to-string duration))))

(defun reject (&key reason)
  (check-type reason (or null (member :rejected :busy)))
  (with-element "Reject"
    (when reason
      (attribute "reason" (string-downcase reason)))))



(defun message (body-text &key to from action method
                               media)
  (check-type body-text string)
  (assert (>= 1600 (length body-text)))
  (check-type media (or string puri:uri cons))
  (when (consp media)
    (assert (>= 10 (length media))))
  (check-type action (or null string puri:uri))
  (check-type method (or null (member :get :post)))
  ;; action gets MessageStatus ∈ queued, sending, sent, or failed
  (with-element "Message"
    (when to (attribute "to" to))
    (when from (attribute "from" from))
    (when (< 0 (length (string-trim tootsville::+whitespace+ body-text)))
      (with-element "Body" (text body-text)))
    (when action (attribute "action" (etypecase action
                                       (string action)
                                       (puri:uri (puri:render-uri action nil)))))
    (when method (attribute "method" (string method)))
    (when media
      (etypecase media
        (string (with-element "Media" (text media)))
        (puri:uri (with-element "Media" (text (puri:render-uri media nil))))
        (cons (dolist (item media)
                (with-element "Media"
                  (etypecase item
                    (string item)
                    (puri:uri (puri:render-uri item nil))))))))))
